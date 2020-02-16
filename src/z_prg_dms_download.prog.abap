*&---------------------------------------------------------------------*
*& Report Z_PRG_DMS_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_prg_dms_download.

* -- global data -- *
data: dms_doc type draw-doknr,
      obj_key type drad-objky.

* -- selection screen -- *
selection-screen begin of block sel with frame title text-sel.
select-options: s_doknr for dms_doc,
                s_objky for obj_key.
selection-screen end of block sel.
selection-screen begin of block opt with frame title text-opt.
parameters: r_fil radiobutton group rad default 'X',
            r_zip radiobutton group rad.
selection-screen end of block opt.

* -- local class definition -- *
class lcl_application definition final.
  public section.
    methods: process.

  private section.
    data: lv_flg_no_input type bool.
    data: selected_folder type string.
    methods: check_input, get_output_path, dwnld_dms_files.
endclass.

class main definition.
  public section.
    class-methods: start.
endclass.

* -- local class implementation -- *
class lcl_application implementation.
  method process.
    check_input( ).
    if lv_flg_no_input eq abap_false.
      get_output_path( ).
      if selected_folder is not initial.
        dwnld_dms_files( ).
      else.
        message 'No output path specified' type 'S' display like 'E'.
      endif.
    else.
      return.
    endif.
  endmethod.

  method check_input.
    if s_doknr is initial and s_objky is initial.
      clear lv_flg_no_input.
      lv_flg_no_input = abap_true.
      message 'Provide atleast one input' type 'S' display like 'E'.
      return.
    endif.
  endmethod.

  method get_output_path.
    data window_title    type string value 'Select output folder'.

    data initial_folder type string.
    clear initial_folder.
    cl_gui_frontend_services=>get_desktop_directory(
    changing
      desktop_directory     = initial_folder
    exceptions
      cntl_error            = 1
      error_no_gui          = 2
      not_supported_by_gui  = 3
      others                = 0 ).

    cl_gui_cfw=>flush( exceptions cntl_error = 1 cntl_system_error = 2 others = 3 ).

    clear selected_folder.
    call method cl_gui_frontend_services=>directory_browse
      exporting
        window_title         = window_title
        initial_folder       = initial_folder
      changing
        selected_folder      = selected_folder
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4.
    if sy-subrc <> 0.
*     Implement suitable error handling here
    endif.
  endmethod.

  method dwnld_dms_files.
    data it_document_numbers  type zcl_helper=>tty_document_number.
    data it_object_keys       type zcl_helper=>tty_object_key.
    data iv_dwnld_folder_path type string.
    data iv_compressed        type boolean.
    data iv_uncompressed      type boolean.

    refresh: it_document_numbers, it_object_keys.
    clear: iv_dwnld_folder_path, iv_compressed, iv_uncompressed.

    if r_fil eq abap_true.
      iv_uncompressed = abap_true.
    elseif r_zip eq abap_true.
      iv_compressed = abap_true.
    endif.

    it_document_numbers = conv zcl_helper=>tty_document_number( s_doknr[] ).
    it_object_keys = conv zcl_helper=>tty_object_key( s_objky[] ).

    iv_dwnld_folder_path = selected_folder.

    call method zcl_helper=>dms_download_utility
      exporting
        it_document_numbers  = it_document_numbers
        it_object_keys       = it_object_keys
        iv_dwnld_folder_path = iv_dwnld_folder_path
        iv_uncompressed      = iv_uncompressed
        iv_compressed        = iv_compressed
      exceptions
        invalid_output_mode  = 1
        empty_input          = 2
        no_output_folder     = 3
        others               = 4.
    if sy-subrc <> 0.
*     Implement suitable error handling here
    endif.

  endmethod.
endclass.

class main implementation.
  method start.
    data(lo_app) = new lcl_application( ).

    check lo_app is bound.
    lo_app->process( ).
  endmethod.
endclass.

* -- selection screen events -- *

* -- start of selection/main -- *
start-of-selection.
  main=>start( ).
