#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/of_dma.h>
#include <linux/slab.h>
#include <linux/of_device.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/dma-mapping.h>
#include <linux/mutex.h>
#include <linux/dmaengine.h>
#include <linux/wait.h>
#include <linux/poll.h>
#include <asm/uaccess.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Dan Sumorok");
MODULE_DESCRIPTION("Reed Solomon Driver");

typedef struct rsDriverPriv_s {
  struct dma_chan *dmaToFPGA;
  struct dma_chan *dmaFromFPGA;
  struct device *dev;
  struct class *deviceClass;
  struct cdev cdev;
  struct dma_async_tx_descriptor *toFPGADesc;
  struct dma_async_tx_descriptor *fromFPGADesc;
  size_t dmaBufferLength;
  dev_t devNum;
  size_t inBlockSize;
  size_t outBlockSize;
  bool deviceCreated;
  bool cdevCreated;
  bool workComplete;
  struct mutex lock;
  enum dma_status dmaStatus;
  wait_queue_head_t workQueue;
  size_t dmaAlign;
} rsDriverPriv_t;

typedef struct rsFileData_s {
  rsDriverPriv_t *priv;
  wait_queue_head_t writeQueue;
  wait_queue_head_t readQueue;
  bool writeSuccess;
  bool writePending;
  size_t readLength;
  uint8_t *dmaBufferFromFPGA;
  dma_addr_t dmaBufferFromFPGAPhys;
  uint8_t *dmaBufferToFPGA;
  dma_addr_t dmaBufferToFPGAPhys;
} rsFileData_t;

static void cleanup(rsDriverPriv_t *priv);
static void cleanup_file_data(rsFileData_t *fileData);
static int rs_remove(struct platform_device *pdev);
static int rs_probe(struct platform_device *pdev);
static unsigned int rs_driver_poll(struct file *, struct poll_table_struct *);
static int rs_driver_open(struct inode *inod, struct file *fil);
static int rs_driver_release(struct inode *inod, struct file *fil);
static ssize_t rs_driver_read(struct file *fil, char *buf, size_t len,
                              loff_t *offset);
static ssize_t rs_driver_write(struct file *fil, const char *buf, size_t len,
                               loff_t *offset);
static long rs_driver_ioctl(struct file *fil, unsigned int cmd,
                            unsigned long arg);
static void rs_dma_callback(void *dma_async_param);

static struct file_operations fops = {
  .owner            = THIS_MODULE,
  .open             = rs_driver_open,
  .release          = rs_driver_release,
  .read             = rs_driver_read,
  .write            = rs_driver_write,
  .unlocked_ioctl   = rs_driver_ioctl,
  .poll             = rs_driver_poll
};


/**
 * rs_remove - Driver remove function
 * @pdev: Pointer to the platform_device structure
 *
 * Return: Always '0'
 */
static int rs_remove(struct platform_device *pdev) {
  rsDriverPriv_t *priv = platform_get_drvdata(pdev);

  if(priv != NULL) {
    cleanup(priv);

    dev_info(priv->dev, "Shutting down\n");
  }

  return 0;
}

static void cleanup(rsDriverPriv_t *priv) {
  if(priv == NULL) {
    return;
  }

  if(priv->cdevCreated) {
    cdev_del(&priv->cdev);
    priv->cdevCreated = false;
  }

  if(priv->deviceCreated) {
    device_destroy(priv->deviceClass, priv->devNum);
    priv->deviceCreated = false;
  }

  if(priv->deviceClass != NULL) {
    class_destroy(priv->deviceClass);
    priv->deviceClass = NULL;
  }

  if(priv->devNum != 0) {
    unregister_chrdev_region(priv->devNum, 1);
    priv->devNum = 0;
  }

  if(priv->dmaFromFPGA != NULL) {
    dma_release_channel(priv->dmaFromFPGA);
    priv->dmaFromFPGA = NULL;
  }

  if(priv->dmaToFPGA != NULL) {
    dma_release_channel(priv->dmaToFPGA);
    priv->dmaToFPGA = NULL;
  }
}

/**
 * rs_probe - Driver probe function
 * @pdev: Pointer to the platform_device structure
 *
 * Return: '0' on success and failure value on error
 */
static int rs_probe(struct platform_device *pdev) {
  struct device_node *node = NULL;
  rsDriverPriv_t *priv = NULL;
  int err = -EIO;
  const char *devName = NULL;
  const size_t dmaAlign = 32;
  const size_t dmaBufferLength = 1024UL * 1024UL;
  const size_t dmaMaskBits = 32;
  u32 blockSize = 0;

  do {
    node = pdev->dev.of_node;

    if(node == NULL) {
      dev_err(&pdev->dev, "device tree node == NULL!\n");
      err = -EINVAL;
      break;
    }

    /* Retreive the device name */
    if(of_property_read_string(node, "devname", &devName) != 0) {
      dev_err(&pdev->dev, "Failed to get device name\n");
      err = -EINVAL;
      break;
    }

    /* Allocate state for this driver */
    priv = devm_kzalloc(&pdev->dev, sizeof(rsDriverPriv_t), GFP_KERNEL);
    if(priv == NULL) {
      dev_err(&pdev->dev, "Failed to allocate\n");
      err = -ENOMEM;
      break;
    }

    priv->dev = &(pdev->dev);
    platform_set_drvdata(pdev, priv);
    mutex_init(&priv->lock);
    init_waitqueue_head(&priv->workQueue);

    if(of_property_read_u32(node, "in-block-size", &blockSize) != 0) {
      dev_err(priv->dev, "Failed to read in block size (%d)\n", (int)blockSize);
      err = -EINVAL;
      break;
    }
    if(blockSize == 0) {
      dev_info(priv->dev, "In block size cannot be zero");
      err = -EINVAL;
      break;
    }
    priv->inBlockSize = (size_t)blockSize;

    if(of_property_read_u32(node, "out-block-size", &blockSize) != 0) {
      dev_err(priv->dev, "Failed to read out block size\n");
      err = -EINVAL;
      break;
    }
    if(blockSize == 0) {
      dev_info(priv->dev, "Out block size cannot be zero");
      err = -EINVAL;
      break;
    }
    priv->outBlockSize = (size_t)blockSize;

    /* Setting the DMA mask tells the system which address range we
       support for DMA. */
    if(dma_set_mask_and_coherent(priv->dev, DMA_BIT_MASK(dmaMaskBits)) != 0) {
      dev_err(priv->dev, "Failed to set dma mask.\n");
      err = -EIO;
      break;
    }

    priv->dmaBufferLength = dmaBufferLength;
    priv->dmaAlign = dmaAlign;

    /* Find the DMA channel for send data to the device. */
    priv->dmaToFPGA = of_dma_request_slave_channel(node, "toFPGA");
    if(IS_ERR(priv->dmaToFPGA)) {
      dev_err(priv->dev, "Did not find DMA from FPGA\n");
      err = PTR_ERR(priv->dmaToFPGA);
      priv->dmaToFPGA = NULL;
      break;
    }

    /* Find the DMA channel for receive data from the device. */
    priv->dmaFromFPGA = of_dma_request_slave_channel(node, "fromFPGA");
    if(IS_ERR(priv->dmaFromFPGA)) {
      dev_err(priv->dev, "Did not find DMA to FPGA\n");
      err = PTR_ERR(priv->dmaFromFPGA);
      priv->dmaFromFPGA = NULL;
      break;
    }

    if(alloc_chrdev_region(&priv->devNum, 0, 1, devName) < 0) {
      dev_err(priv->dev, "Failed to allocate character device region\n");
      err = -ENOMEM;
      break;
    }

    if((priv->deviceClass = class_create(THIS_MODULE, devName)) == NULL) {
      dev_err(priv->dev, "Failed to create device class\n");
      err = -ENOMEM;
      break;
    }

    if(device_create(priv->deviceClass, NULL, priv->devNum,
                     NULL, devName) == NULL) {
      dev_err(priv->dev, "Failed to create device");
      err = -ENOMEM;
      break;
    }
    priv->deviceCreated = true;

    cdev_init(&priv->cdev, &fops);

    err = cdev_add(&priv->cdev, priv->devNum, 1);
    if(err != 0) {
      dev_err(priv->dev, "cdev_add() failed\n");
      break;
    }
    priv->cdevCreated = true;

    /* Initialize DMA descriptors */

    dev_info(priv->dev, "Starting up.\n");

    return 0;
  } while( false ) ;

  cleanup(priv);

  return err;
}

static unsigned int rs_driver_poll(struct file *fil, struct poll_table_struct *pol) {
  rsFileData_t *fileData = fil->private_data;
  unsigned int mask = 0;

  poll_wait(fil, &fileData->readQueue, pol);
  poll_wait(fil, &fileData->writeQueue, pol);

  if(fileData->writePending) {
    mask |= POLLIN | POLLRDNORM;
  } else {
    mask |= POLLOUT | POLLWRNORM;
  }
  
  return mask;
}

static void cleanup_file_data(rsFileData_t *fileData) {
  rsDriverPriv_t *priv;

  if(fileData == NULL) {
    return;
  }

  priv = fileData->priv;

  if(fileData->dmaBufferFromFPGA != NULL) {
    dma_free_coherent(priv->dev, priv->dmaBufferLength,
                      fileData->dmaBufferFromFPGA,
                      fileData->dmaBufferFromFPGAPhys);
    fileData->dmaBufferFromFPGA = NULL;
    fileData->dmaBufferFromFPGAPhys = 0;
  }

  if(fileData->dmaBufferToFPGA != NULL) {
    dma_free_coherent(priv->dev, priv->dmaBufferLength,
                      fileData->dmaBufferToFPGA,
                      fileData->dmaBufferToFPGAPhys);
    fileData->dmaBufferToFPGA = NULL;
    fileData->dmaBufferToFPGAPhys = 0;
  }

  kfree(fileData);
}

static int rs_driver_open(struct inode *inod, struct file *fil) {
  int err = -EIO;
  rsDriverPriv_t *priv = NULL;
  rsFileData_t *fileData = NULL;

  if(!try_module_get(THIS_MODULE)) {
    return -EBUSY;
  }

  do {
    priv = container_of(inod->i_cdev, rsDriverPriv_t, cdev);

    fileData = kmalloc(sizeof(rsFileData_t), GFP_KERNEL);
    if(fileData == NULL) {
      err = -ENOMEM;
      break;
    }

    memset(fileData, 0, sizeof(rsFileData_t));

    fileData->priv = priv;
    init_waitqueue_head(&fileData->writeQueue);
    init_waitqueue_head(&fileData->readQueue);

    fil->private_data = fileData;

    /* Allocate a buffer for DMA.  This returns both the virtual
       address and the physical address. */
    fileData->dmaBufferFromFPGA =
      dma_alloc_coherent(priv->dev, priv->dmaBufferLength,
                         &fileData->dmaBufferFromFPGAPhys,
                         GFP_KERNEL);

    if(fileData->dmaBufferFromFPGA == NULL) {
      dev_err(priv->dev, "Failed to allocate DMA buffer from FPGA\n");
      err = -ENOMEM;
      break;
    }

    /* Ensure the physical address is appropriately aligned.  In
       particular is should probably line up on a cache line
       boundary. */
    if(fileData->dmaBufferFromFPGAPhys & ((dma_addr_t)priv->dmaAlign - 1)) {
      dev_err(priv->dev, "DMA Buffer (0x%zX) is not %u-byte aligned",
              (size_t)fileData->dmaBufferFromFPGAPhys,
              (unsigned int)priv->dmaAlign);
      err = -ENOMEM;
      break;
    }

    /* Allocate a buffer for DMA.  This returns both the virtual
       address and the physical address. */
    fileData->dmaBufferToFPGA =
      dma_alloc_coherent(priv->dev, priv->dmaBufferLength,
                         &fileData->dmaBufferToFPGAPhys,
                         GFP_KERNEL);

    if(fileData->dmaBufferToFPGA == NULL) {
      dev_err(priv->dev, "Failed to allocate DMA buffer to FPGA\n");
      err = -ENOMEM;
      break;
    }

    /* Ensure the physical address is appropriately aligned.  In
       particular is should probably line up on a cache line
       boundary. */
    if(fileData->dmaBufferToFPGAPhys & ((dma_addr_t)priv->dmaAlign - 1)) {
      dev_err(priv->dev, "DMA Buffer (0x%zX) is not %u-byte aligned",
              (size_t)fileData->dmaBufferToFPGAPhys,
              (unsigned int)priv->dmaAlign);
      err = -ENOMEM;
      break;
    }

    return 0;
  } while( false );

  cleanup_file_data(fileData);

  module_put(THIS_MODULE);

  return err;
}

static int rs_driver_release(struct inode *inod, struct file *fil) {
  rsFileData_t *fileData = fil->private_data;

  cleanup_file_data(fileData);

  module_put(THIS_MODULE);

  return 0;
}

static ssize_t rs_driver_read(struct file *fil, char *buf, size_t len,
                              loff_t *offset) {
  int err = -EIO;
  rsFileData_t *fileData = fil->private_data;
  rsDriverPriv_t *priv = fileData->priv;
  size_t readLen = len;

  mutex_lock(&priv->lock);
  do {
    while(!fileData->writePending) {
      mutex_unlock(&priv->lock);
      err = wait_event_interruptible(fileData->readQueue,
                                     fileData->writePending);
      mutex_lock(&priv->lock);
      if(err != 0) {
        break;
      }
    }

    if(!fileData->writeSuccess) {
      err = -EIO;
      break;
    }

    if(fileData->readLength > len) {
      readLen = len;
    } else {
      readLen = fileData->readLength;
    }

    if(copy_to_user(buf, fileData->dmaBufferFromFPGA, readLen) != 0) {
      err = -EFAULT;
      break;
    }

    fileData->writePending = false;
    wake_up(&fileData->writeQueue);
    err = (ssize_t)readLen;
  } while(false);

  mutex_unlock(&priv->lock);

  return err;
}

static ssize_t rs_driver_write(struct file *fil, const char *buf, size_t len,
                               loff_t *offset) {
  rsFileData_t *fileData = fil->private_data;
  rsDriverPriv_t *priv = fileData->priv;
  int err = 0;
  size_t blockCount, outLen;

  /* Sanity Check length */
  if((len % priv->inBlockSize) != 0) {
    dev_err(priv->dev, "Input length must be a multiple of %u",
            (unsigned int)priv->inBlockSize);
    return -EINVAL;
  }

  if(len > priv->dmaBufferLength) {
    dev_err(priv->dev, "Input length too large\n");
    return -EINVAL;
  }

  blockCount = len / priv->inBlockSize;
  outLen = blockCount * priv->outBlockSize;
  if(outLen > priv->dmaBufferLength) {
    dev_err(priv->dev, "Input length too large\n");
    return -EINVAL;
  }

  mutex_lock(&priv->lock);
  do {
    /* Wait for previous output to be read */
    while(fileData->writePending) {
      mutex_unlock(&priv->lock);
      err = wait_event_interruptible(fileData->writeQueue,
                                     !fileData->writePending);
      mutex_lock(&priv->lock);
      if(err != 0) {
        break;
      }
    }

    if(err != 0) {
      break;
    }

    /* Copy user data to dma buffer */
    if(copy_from_user(fileData->dmaBufferToFPGA, buf, len) != 0) {
      err = -EFAULT;
      break;
    }

    priv->workComplete = false;

    /* Set up DMA of input buffer to FPGA */
    priv->toFPGADesc = dmaengine_prep_slave_single(priv->dmaToFPGA,
                                                   fileData->dmaBufferToFPGAPhys,
                                                   len, DMA_TO_DEVICE, 0);
    if(priv->toFPGADesc == NULL) {
      dev_err(priv->dev, "Failed to prepare DMA to FPGA\n");
      break;
    }

    if(dmaengine_submit(priv->toFPGADesc) < 0) {
      dev_err(priv->dev, "Failed to submit dma to FPGA\n");
      break;
    }

    dma_async_issue_pending(priv->dmaToFPGA);

    /* Set up DMA of output buffer from FPGA */
    priv->fromFPGADesc = dmaengine_prep_slave_single(priv->dmaFromFPGA,
                                                     fileData->dmaBufferFromFPGAPhys,
                                                     outLen, DMA_FROM_DEVICE,
                                                     DMA_PREP_INTERRUPT);
    if(priv->fromFPGADesc == NULL) {
      dmaengine_terminate_all(priv->dmaToFPGA);
      dev_err(priv->dev, "Failed to prepare DMA from FPGA\n");
      break;
    }

    priv->fromFPGADesc->callback = rs_dma_callback;
    priv->fromFPGADesc->callback_param = fileData;

    if(dmaengine_submit(priv->fromFPGADesc) < 0) {
      dmaengine_terminate_all(priv->dmaToFPGA);
      dev_err(priv->dev, "Failed to submit dma from FPGA\n");
      break;
    }

    dma_async_issue_pending(priv->dmaFromFPGA);

    /* Wait for DMA to complete */
    if(wait_event_timeout(priv->workQueue,
                          priv->workComplete,
                          HZ) == 0) {
      /* DMA timed out, probably not good */
      err = -EIO;
      break;
    }

    /* Signal reader */
    fileData->readLength = outLen;
    fileData->writePending = true;
    wake_up(&fileData->readQueue);

    err = (ssize_t)len;
  } while( false );

  mutex_unlock(&priv->lock);

  return err;
}

static long rs_driver_ioctl(struct file *fil, unsigned int cmd,
                            unsigned long arg) {
  int err = -EINVAL;

  return err;
}

static void rs_dma_callback(void *dma_async_param) {
  rsFileData_t *fileData = (rsFileData_t *)dma_async_param;
  rsDriverPriv_t *priv = fileData->priv;
  struct dma_tx_state state = {0};

  priv->dmaStatus = dmaengine_tx_status(priv->dmaFromFPGA,
                                        priv->dmaFromFPGA->cookie,
                                        &state);
  if(priv->dmaStatus == DMA_COMPLETE) {
    fileData->writeSuccess = true;
  } else {
    fileData->writeSuccess = false;
  }

  priv->workComplete = true;
  wake_up(&priv->workQueue);
}

static const struct of_device_id reed_solomon_of_match[] = {
        { .compatible = "dsumorok,rs-1.00.a",},
        {}
};
MODULE_DEVICE_TABLE(of, reed_solomon_of_match);

static struct platform_driver reed_solomon_driver = {
        .driver = {
                .name = "reed-solomon",
                .of_match_table = reed_solomon_of_match,
        },
        .probe = rs_probe,
        .remove = rs_remove,
};

module_platform_driver(reed_solomon_driver);
