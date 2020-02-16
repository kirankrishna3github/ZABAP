function zfm_tms_mgr_transmit_tr_queue.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TAR_SYS) TYPE  TMSCSYS-SYSNAM
*"     VALUE(IV_TAR_DOM) TYPE  TMSCDOM-DOMNAM
*"     VALUE(IT_REQUESTS) TYPE  TMSBUFFERS
*"  EXCEPTIONS
*"      READ_CONFIG_FAILED
*"      SYSTEM_NOT_FOUND
*"      GROUP_NOT_FOUND
*"      NO_SOURCE_SYSTEMS_FOUND
*"      FEATURE_NOT_AVAILABLE
*"      IDENTICAL_GROUPS
*"      CHECK_GROUP_CONFIG_FAILED
*"      INVALID_GROUP_CONFIG
*"      OTHERS
*"----------------------------------------------------------------------

  call function 'TMS_MGR_TRANSMIT_TR_QUEUE'
    exporting
      iv_tar_sys                = iv_tar_sys
      iv_tar_dom                = iv_tar_dom
      iv_read_only              = space
      it_requests               = it_requests
    exceptions
      read_config_failed        = 1
      system_not_found          = 2
      group_not_found           = 3
      no_source_systems_found   = 4
      feature_not_available     = 5
      identical_groups          = 6
      check_group_config_failed = 7
      invalid_group_config      = 8
      others                    = 9.

  case sy-subrc.
    when 0.
    when 1.
      raise read_config_failed.
    when 2.
      raise system_not_found.
    when 3.
      raise group_not_found.
    when 4.
      raise no_source_systems_found.
    when 5.
      raise feature_not_available.
    when 6.
      raise identical_groups.
    when 7.
      raise check_group_config_failed.
    when 8.
      raise invalid_group_config.
    when others.
      raise others.
  endcase.

endfunction.
