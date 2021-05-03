*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

define add_message.
  clear lv_message.
  message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_message.

  append conv #( lv_message ) to et_message.
end-of-definition.

define addr_memory_clear.
  call function 'ADDR_MEMORY_CLEAR'
    exporting
      force              = abap_true
    exceptions
      unsaved_data_exist = 1
      internal_error     = 2
      others             = 3.
  if sy-subrc <> 0.
    add_message.
  endif.

*  " OR - Clear equivalent for save_single
*  call function 'ADDR_SINGLE_RESET'
*    exporting
*      address_number      = rv_addr_number(&1) " Address Number (Key of Database Table)
*      address_handle      = lv_addr_handle(&2) " Address handle (temporary key)
*    exceptions
*      number_not_found    = 1     " Address number not in local memory
*      handle_not_found    = 2     " Adress handle not in local memory
*      parameter_error     = 3     " Incorrect parameter values
*      internal_error      = 4     " Serious internal error
*      unsaved_data_exists = 5     " Address has already been changed
*      others              = 6.
*  if sy-subrc <> 0.
*    add_message.
*  endif.
end-of-definition.
