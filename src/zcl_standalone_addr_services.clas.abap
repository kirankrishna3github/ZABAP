class ZCL_STANDALONE_ADDR_SERVICES definition
  public
  final
  create public .

public section.

  types:
    mtty_message type standard table of bapi_msg .

  class-methods CREATE_SINGLE
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    changing
      value(CS_ADDR_VAL) type ADDR1_VAL .
  class-methods READ_SINGLE
    importing
      value(IV_ADDR_NUMBER) type ADRC-ADDRNUMBER
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    returning
      value(RS_ADDR_VAL) type ADDR1_VAL .
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
  class-data MV_MESSAGE type BAPI_MSG .

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
      lv_rc_addr_num_get type inri-returncode.

    clear et_message.

    if cs_addr_val is initial.
      append conv #( 'Please supply the address data' ) to et_message.
      return.
    endif.

    if cs_addr_val-addr_group is initial.
      append conv #( 'Please specify an address group' ) to et_message.
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
        address_data        = cs_addr_val
        address_group       = cs_addr_val-addr_group  " Address group (key)
        address_handle      = lv_addr_handle " Address handle (temporary key)
        check_empty_address = abap_false
        check_address       = abap_false            " Flag: Check address contents
      importing
        address_data        = cs_addr_val
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
              clear mv_message.
              message id ls_addr_error-msg_id type ls_addr_error-msg_type number ls_addr_error-msg_number
                with ls_addr_error-msg_var1 ls_addr_error-msg_var2 ls_addr_error-msg_var3 ls_addr_error-msg_var4 into mv_message.

              append mv_message to et_message.
              clear ls_addr_error.
            endloop.
          endif.
          addr_memory_clear. " or addr_memory_clear_single '' lv_addr_handle.
          return.
        else.
          clear lv_rc_addr_num_get.

          call function 'ADDR_NUMBER_GET'
            exporting
              address_handle           = lv_addr_handle             " Address Handle (Temporary Key)
              address_reference        = value addr_ref( appl_table = lv_appl_table
                                                         appl_field = lv_appl_field
                                                         appl_key   = lv_appl_key
                                                         addr_group = cs_addr_val-addr_group
                                                         owner      = abap_true )          " Address reference (fill correctly -> see long text)
            importing
              address_number           = cs_addr_val-addrnumber     " Number taken from the number range
              returncode_numberrange   = lv_rc_addr_num_get         " Return code when taking number
            exceptions
              address_handle_not_exist = 1                          " Transferred address handle does not exist
              internal_error           = 2                          " Serious internal error (MESSAGE A...)
              parameter_error          = 3                          " Incorrect parameter values
              others                   = 4.
          if sy-subrc <> 0 or cs_addr_val-addrnumber is initial or lv_rc_addr_num_get is not initial.
            add_message.
            addr_memory_clear.  " or addr_memory_clear_single '' lv_addr_handle.
            return.
          else.
            if cs_addr_val-addrnumber is not initial.
              addr_memory_save_single cs_addr_val-addrnumber. " or addr_memory_save.
            endif.
          endif.
        endif.
      when others.
        add_message.
        addr_memory_clear.  " or addr_memory_clear_single '' lv_addr_handle.
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
    data:
      lt_addr_error  type standard table of addr_error,
      lv_rc_addr_get type szad_field-returncode.

    clear rs_addr_val.

    if iv_addr_number is initial.
      append conv #( 'Please specify an address number' ) to et_message.
      return.
    endif.

    addr_memory_clear.

    clear:
      lt_addr_error,
      lv_rc_addr_get.

    call function 'ADDR_GET'
      exporting
        address_selection = value addr1_sel( addrnumber = iv_addr_number )
*       blk_excpt         = abap_true   " this will cause NO data to be read if adrc-xpcpt = 'X',
        " but that is respected only when the business function
        " BUPA_ILM_IF is activated via SFW5
      importing
        address_value     = rs_addr_val
        returncode        = lv_rc_addr_get
      tables
        error_table       = lt_addr_error
      exceptions
        parameter_error   = 1
        address_not_exist = 2
        version_not_exist = 3
        internal_error    = 4
        address_blocked   = 5
        others            = 6.
    case sy-subrc.
      when 0.
        if lv_rc_addr_get = 'E'.
          if lt_addr_error is not initial.
            loop at lt_addr_error into data(ls_addr_error).
              clear mv_message.
              message id ls_addr_error-msg_id type ls_addr_error-msg_type number ls_addr_error-msg_number
                with ls_addr_error-msg_var1 ls_addr_error-msg_var2 ls_addr_error-msg_var3 ls_addr_error-msg_var4 into mv_message.

              append mv_message to et_message.
              clear ls_addr_error.
            endloop.
            addr_memory_clear.
            return.
          endif.
        endif.
      when others.
        add_message.
        addr_memory_clear.
        return.
    endcase.

    addr_memory_clear.
  endmethod.


  method UNBLOCK_ADDRESS.
  endmethod.


  method UNLOCK_ADDRESS.
  endmethod.


  method UPDATE_SINGLE.
  endmethod.
ENDCLASS.
