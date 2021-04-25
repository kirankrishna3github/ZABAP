initialization.
  " placeholder

at selection-screen output.
  lcl_app=>sel_screen_pbo( ).

at selection-screen on value-request for p_file.
  p_file = zcl_helper=>file_selection_dialog(
             exporting                             " Selection Dialog Title
               iv_file_filter = cl_gui_frontend_services=>filetype_excel ). " File Filter


at selection-screen.
  lcl_app=>sel_screen_pai( ).
