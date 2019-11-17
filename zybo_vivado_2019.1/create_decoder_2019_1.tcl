create_project rsDecoder rsDecoder -part xc7z010clg400-1
set_property target_language VHDL [current_project]
import_files -fileset sources_1 -norecurse { ../vhdl/gf.vhd \
						 ../vhdl/rsUtil.vhd \
						 ../vhdl/mea.vhd \
						 ../vhdl/chien_search.vhd \
						 ../vhdl/correctErrors.vhd \
						 ../vhdl/rs_ram.vhd \
						 ../vhdl/calcSyndromes.vhd \
						 ../vhdl/calcErrors.vhd \
						 ../vhdl/rsDecoder.vhd \
						 ../vhdl/rsFeeder.vhd \
						 ../vhdl/rsDecoderWrapper.vhd \
						 ../vhdl/rsFifo.vhd \
						 ../vhdl/rsOutputStage.vhd \
						 ../vhdl/start2valid.vhd }

set_property FILE_TYPE {VHDL 2008} [get_files *.vhd]

update_compile_order -fileset sources_1
set_property top rsDecoderWrapper [get_filesets sources_1]

ipx::package_project -root_dir ip_root/rsDecoder -vendor user.org -library user -taxonomy /UserIP -import_files
set_property vendor_display_name {Dan Sumorok} [ipx::current_core]
set_property vendor {dsumorok.com} [ipx::current_core]
set_property core_revision 2 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]
set_property  ip_repo_paths ip_root/rsDecoder [current_project]
update_ip_catalog

close_project
exit
