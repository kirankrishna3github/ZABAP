function zfm_tms_mgr_import_tr_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SYSTEM) TYPE  TMSCSYS-SYSNAM
*"     VALUE(IV_DOMAIN) TYPE  TMSCDOM-DOMNAM
*"     VALUE(IT_REQUESTS) TYPE  TMSBUFFERS
*"  EXCEPTIONS
*"      READ_CONFIG_FAILED
*"      TABLE_OF_REQUESTS_IS_EMPTY
*"      OTHERS
*"----------------------------------------------------------------------

  call function 'TMS_MGR_IMPORT_TR_REQUEST'
    exporting
      iv_system                  = iv_system
      iv_domain                  = iv_domain
      iv_request                 = 'SOME'
      iv_client                  = '300'
      iv_overtake                = 'X'
      iv_import_again            = 'X'
      iv_ignore_originality      = 'X'
      iv_ignore_repairs          = 'X'
      iv_ignore_cvers            = 'X'
      iv_test_import             = ''
      it_requests                = it_requests
    exceptions
      read_config_failed         = 1
      table_of_requests_is_empty = 2
      others                     = 3.

  case sy-subrc.
    when 0.
    when 1.
      raise read_config_failed.
    when 2.
      raise table_of_requests_is_empty.
    when others.
      raise others.
  endcase.
endfunction.
