function-pool zfg_xx_pdf_viewer.            "MESSAGE-ID ..

* INCLUDE LZFG_XX_PDF_VIEWERD...             " Local class definition

data:
  gv_pdf         type xstring,
  gv_display     type abap_bool,
  gv_print       type abap_bool,
  go_html_viewer type ref to cl_gui_html_viewer.
