*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

define add_message.
  clear mv_message.
  message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into mv_message.

  append mv_message to et_message.
end-of-definition.

define addr_memory_clear.
  " clear the entire local memory of function group SZA0
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
end-of-definition.

define addr_memory_clear_single.
  " clear the local memory of function group SZA0 pertaining to one address/handle
  call function 'ADDR_SINGLE_RESET'
    exporting
      address_number      = &1    " Address Number (Key of Database Table)
      address_handle      = &2    " Address handle (temporary key)
    exceptions
      number_not_found    = 1     " Address number not in local memory
      handle_not_found    = 2     " Adress handle not in local memory
      parameter_error     = 3     " Incorrect parameter values
      internal_error      = 4     " Serious internal error
      unsaved_data_exists = 5     " Address has already been changed
      others              = 6.
  if sy-subrc <> 0.
    add_message.
  endif.
end-of-definition.

define addr_memory_save.
  " Persist the entire local memory of function group SZA0 to DB
  call function 'ADDR_MEMORY_SAVE'
    exceptions
      address_number_missing = 1     " Missing Address Number for New Address
      person_number_missing  = 2     " Missing Person Number for New Person
      internal_error         = 3     " Serious internal error (MESSAGE A...)
      database_error         = 4     " Error when Writing to the Database (Dialog)
      reference_missing      = 5
      others                 = 6.
  if sy-subrc <> 0.
    add_message.
    addr_memory_clear.
    return.
  else.
    commit work.
  endif.
end-of-definition.

define addr_memory_save_single.
  " Persist the local memory of function group SZA0 to DB pertaining to one address
  call function 'ADDR_SINGLE_SAVE'
    exporting
      address_number         = &1   " Address Number
    exceptions
      address_not_exist      = 1    " Address Not in Local Memory
      person_not_exist       = 2    " Person not found
      address_number_missing = 3    " Missing Address Number for New Address
      reference_missing      = 4    " Missing Reference
      internal_error         = 5    " Serious internal error (MESSAGE A...)
      database_error         = 6    " Error when Writing to the Database (Dialog)
      parameter_error        = 7    " Invalid Parameter Value (for Example, Blank Number)
      others                 = 8.
  if sy-subrc <> 0.
    add_message.

    if &2 = abap_true.
      unlock_address(
        exporting
          iv_addr_number = &1 ).
    endif.

    addr_memory_clear.
    return.
  else.
    commit work.
  endif.
end-of-definition.
