class ZCL_STANDALONE_ADDR_SERVICES definition
  public
  final
  create public .

public section.

  types:
    mtty_message type standard table of bapi_msg .

  class-methods CREATE_SINGLE
    importing
      value(IV_ADDR_GROUP) type ADRG-ADDR_GROUP
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    changing
      value(CS_ADDR_DATA) type ADDR1_DATA
    returning
      value(RV_ADDR_NUMBER) type ADRC-ADDRNUMBER .
  class-methods READ_SINGLE
    importing
      value(IV_ADDR_NUMBER) type ADRC-ADDRNUMBER
    returning
      value(RS_ADDR_DATA) type ADDR1_DATA .
  class-methods UPDATE_SINGLE .
  class-methods DELETE_SINGLE .
  class-methods MAINTAIN_VIA_POPUP_DIALOG .
  class-methods MAINTAIN_VIA_FULLSCREEN_DIALOG .
  class-methods ADD_REFERENCE .
  class-methods BLOCK_ADDRESS .
  class-methods DELETE_REFERENCE .
  class-methods UNBLOCK_ADDRESS .
protected section.
private section.

  constants MC_DEFAULT_APPL_TABLE type ADDR_REF-APPL_TABLE value 'ADRC' ##NO_TEXT.
  constants MC_DEFAULT_APPL_FIELD type ADDR_REF-APPL_FIELD value 'ADDRNUMBER' ##NO_TEXT.

  class-methods LOCK_ADDRESS .
  class-methods UNLOCK_ADDRESS .
  class-methods GET_ADDRESS_HANDLE
    exporting
      value(EV_APPL_TABLE) type ADDR_REF-APPL_TABLE
      value(EV_APPL_FIELD) type ADDR_REF-APPL_FIELD
      value(EV_APPL_KEY) type ADDR_REF-APPL_KEY
    returning
      value(RV_ADDR_HANDLE) type SZAD_FIELD-HANDLE .
ENDCLASS.



CLASS ZCL_STANDALONE_ADDR_SERVICES IMPLEMENTATION.


  method ADD_REFERENCE.
  endmethod.


  method BLOCK_ADDRESS.
  endmethod.


  method create_single.

    data:
      lt_addr_error      type standard table of addr_error,
      lv_rc_addr_insert  type szad_field-returncode,
      lv_rc_addr_num_get type inri-returncode,
      lv_message         type bapi_msg.

    clear:
      et_message,
      rv_addr_number.

    if iv_addr_group is initial.
      append conv #( 'Please specify an address group' ) to et_message.
      return.
    endif.

    if cs_addr_data is initial.
      append conv #( 'Please supply the address data' ) to et_message.
      return.
    endif.

    addr_memory_clear.

    clear:
      lt_addr_error,
      lv_rc_addr_insert.

    get_address_handle(
      importing
        ev_appl_table = data(lv_appl_table)   " Application table logical name (address management)
        ev_appl_field = data(lv_appl_field)   " Application table address where-used list logical field name
        ev_appl_key   = data(lv_appl_key)
      receiving
        rv_addr_handle = data(lv_addr_handle) ).   " Application table key (incl. client)

    call function 'ADDR_INSERT'
      exporting
        address_data        = cs_addr_data
        address_group       = iv_addr_group  " Address group (key)
        address_handle      = lv_addr_handle " Address handle (temporary key)
        check_empty_address = abap_false
        check_address       = abap_false            " Flag: Check address contents
      importing
        address_data        = cs_addr_data
        returncode          = lv_rc_addr_insert
      tables
        error_table         = lt_addr_error         " Table with errors, warnings, information
      exceptions
        address_exists      = 1
        parameter_error     = 2              " Incorrect parameter values
        internal_error      = 3              " Serious internal error (MESSAGE A...)
        others              = 4.
    case sy-subrc.
      when 0.
        if lv_rc_addr_insert = 'E'.
          if lt_addr_error is not initial.
            loop at lt_addr_error into data(ls_addr_error).
              clear lv_message.
              message id ls_addr_error-msg_id type ls_addr_error-msg_type number ls_addr_error-msg_number
                with ls_addr_error-msg_var1 ls_addr_error-msg_var2 ls_addr_error-msg_var3 ls_addr_error-msg_var4 into lv_message.

              append conv #( lv_message ) to et_message.
              clear ls_addr_error.
            endloop.
          endif.
          addr_memory_clear.
          return.
        else.
          clear lv_rc_addr_num_get.

          call function 'ADDR_NUMBER_GET'
            exporting
              address_handle           = lv_addr_handle             " Address Handle (Temporary Key)
              address_reference        = value addr_ref( appl_table = lv_appl_table
                                                         appl_field = lv_appl_field
                                                         appl_key   = lv_appl_key
                                                         addr_group = iv_addr_group
                                                         owner      = abap_true )          " Address reference (fill correctly -> see long text)
            importing
              address_number           = rv_addr_number             " Number taken from the number range
              returncode_numberrange   = lv_rc_addr_num_get         " Return code when taking number
            exceptions
              address_handle_not_exist = 1                          " Transferred address handle does not exist
              internal_error           = 2                          " Serious internal error (MESSAGE A...)
              parameter_error          = 3                          " Incorrect parameter values
              others                   = 4.
          if sy-subrc <> 0 or rv_addr_number is initial or lv_rc_addr_num_get is not initial.
            add_message.
            addr_memory_clear.
            return.
          else.
            if rv_addr_number is not initial.
              call function 'ADDR_SINGLE_SAVE'
                exporting
                  address_number         = rv_addr_number " Address Number
                exceptions
                  address_not_exist      = 1              " Address Not in Local Memory
                  person_not_exist       = 2              " Person not found
                  address_number_missing = 3              " Missing Address Number for New Address
                  reference_missing      = 4              " Missing Reference
                  internal_error         = 5              " Serious internal error (MESSAGE A...)
                  database_error         = 6              " Error when Writing to the Database (Dialog)
                  parameter_error        = 7              " Invalid Parameter Value (for Example, Blank Number)
                  others                 = 8.
              if sy-subrc <> 0.
                add_message.
                addr_memory_clear.
                return.
              else.
                commit work.
              endif.
            endif.
          endif.
        endif.
      when others.
        add_message.
        addr_memory_clear.
        return.
    endcase.

    addr_memory_clear.
  endmethod.


  method DELETE_REFERENCE.
  endmethod.


  method DELETE_SINGLE.
  endmethod.


  method get_address_handle.
    clear:
      ev_appl_table,
      ev_appl_field,
      ev_appl_key,
      rv_addr_handle.

    ev_appl_table = mc_default_appl_table.
    ev_appl_field = mc_default_appl_field.
    ev_appl_key = sy-mandt + cl_system_uuid=>create_uuid_c22_static( ).

    rv_addr_handle = ev_appl_table && ev_appl_field && ev_appl_key.
  endmethod.


  method LOCK_ADDRESS.
  endmethod.


  method MAINTAIN_VIA_FULLSCREEN_DIALOG.
  endmethod.


  method MAINTAIN_VIA_POPUP_DIALOG.
  endmethod.


  method read_single.
    clear rs_addr_data.

    if iv_addr_number is initial.

    endif.
  endmethod.


  method UNBLOCK_ADDRESS.
  endmethod.


  method UNLOCK_ADDRESS.
  endmethod.


  method UPDATE_SINGLE.
  endmethod.
ENDCLASS.
