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
      value(CS_ADDR_VAL) type ADDR1_VAL
    returning
      value(RV_CREATED) type ABAP_BOOL .
  class-methods READ_SINGLE
    importing
      value(IV_ADDR_NUMBER) type ADRC-ADDRNUMBER
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    returning
      value(RS_ADDR_VAL) type ADDR1_VAL .
  class-methods UPDATE_SINGLE
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    changing
      value(CS_ADDR_VAL) type ADDR1_VAL
    returning
      value(RV_UPDATED) type ABAP_BOOL .
  class-methods DELETE_SINGLE .
  class-methods MAINTAIN_VIA_POPUP_DIALOG .
  class-methods MAINTAIN_VIA_FULLSCREEN_DIALOG .
  class-methods ADD_REFERENCE .
  class-methods BLOCK_ADDRESS .
  class-methods DELETE_REFERENCE .
  class-methods UNBLOCK_ADDRESS .
  class-methods READ_REFERENCES .
protected section.
private section.

  constants MC_DEFAULT_APPL_TABLE type ADDR_REF-APPL_TABLE value 'ADRC' ##NO_TEXT.
  constants MC_DEFAULT_APPL_FIELD type ADDR_REF-APPL_FIELD value 'ADDRNUMBER' ##NO_TEXT.
  class-data MV_MESSAGE type BAPI_MSG .

  class-methods LOCK_ADDRESS
    importing
      value(IV_ADDR_NUMBER) type ADRC-ADDRNUMBER
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    returning
      value(RV_LOCKED) type ABAP_BOOL .
  class-methods UNLOCK_ADDRESS
    importing
      value(IV_ADDR_NUMBER) type ADRC-ADDRNUMBER
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    returning
      value(RV_UNLOCKED) type ABAP_BOOL .
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

    clear:
      et_message,
      rv_created.

    if cs_addr_val is initial.
      append conv #( 'Please supply the address data' ) to et_message.
      return.
    endif.

    if cs_addr_val-addr_group is initial.
      append conv #( 'Please specify an address group' ) to et_message.
      return.
    endif.

    clear cs_addr_val-addrnumber.

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

    data(ls_addr_data) = corresponding addr1_data( cs_addr_val ).

    call function 'ADDR_INSERT'
      exporting
        address_data        = ls_addr_data
        address_group       = cs_addr_val-addr_group  " Address group (key)
        address_handle      = lv_addr_handle " Address handle (temporary key)
        check_empty_address = abap_false
        check_address       = abap_false            " Flag: Check address contents
      importing
        address_data        = ls_addr_data
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
          cs_addr_val = corresponding #( ls_addr_data ).
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
              addr_memory_save_single cs_addr_val-addrnumber abap_false. " or addr_memory_save.
              rv_created = abap_true.
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


  method lock_address.
    clear:
      et_message,
      rv_locked.

    if iv_addr_number is initial.
      append conv #( 'Please specify an address number' ) to et_message.
      return.
    endif.

    if iv_addr_number is not initial.
      call function 'ADDR_ENQUEUE'
        exporting
          address_number    = conv adrc-addrnumber( |{ iv_addr_number alpha = in }| ) " Address Number
        exceptions
          address_not_exist = 1              " Address does not exist
          foreign_lock      = 2              " Address is locked by another user
          system_failure    = 3              " Internal error in lock management
          internal_error    = 4
          others            = 5.
      if sy-subrc <> 0.
        add_message.
        return.
      else.
        rv_locked = abap_true.
      endif.
    endif.
  endmethod.


  method MAINTAIN_VIA_FULLSCREEN_DIALOG.
  endmethod.


  method MAINTAIN_VIA_POPUP_DIALOG.
  endmethod.


  method READ_REFERENCES.
  endmethod.


  method read_single.
    data:
      lt_addr_error  type standard table of addr_error,
      lv_rc_addr_get type szad_field-returncode.

    clear:
      et_message,
      rs_addr_val.

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
        address_selection = value addr1_sel( addrnumber = |{ iv_addr_number alpha = in }| )
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


  method unlock_address.
    clear:
      et_message,
      rv_unlocked.

    if iv_addr_number is initial.
      append conv #( 'Please specify an address number' ) to et_message.
      return.
    endif.

    if iv_addr_number is not initial.
      call function 'ADDR_DEQUEUE'
        exporting
          address_number    = conv adrc-addrnumber( |{ iv_addr_number alpha = in }| )  " Address Number
        exceptions
          address_not_exist = 1              " Address does not exist
          internal_error    = 2
          others            = 3.
      if sy-subrc <> 0.
        add_message.
        return.
      else.
        rv_unlocked = abap_true.
      endif.
    endif.
  endmethod.


  method update_single.
    data:
      lv_rc_addr_update type szad_field-returncode,
      lv_data_changed   type t_boole,
      lt_addr_error     type standard table of addr_error,
      lt_message        type mtty_message.

    clear:
      et_message,
      rv_updated.

    if cs_addr_val is initial.
      append conv #( 'Please supply the address data' ) to et_message.
      return.
    endif.

    if cs_addr_val-addrnumber is initial.
      append conv #( 'Please supply an address number' ) to et_message.
      return.
    endif.

    cs_addr_val-addrnumber = |{ cs_addr_val-addrnumber alpha = in }|.

    define unlock_address.
      clear lt_message.
      if not unlock_address(
               exporting
                 iv_addr_number = cs_addr_val-addrnumber " Address number
               importing
                 et_message     = lt_message ).
        append lines of lt_message to et_message.
      endif.
    end-of-definition.

    addr_memory_clear.

    clear lt_message.
    if lock_address(
         exporting
           iv_addr_number = cs_addr_val-addrnumber " Address number
         importing
           et_message     = lt_message ).

      " process only the supplied fields, keep db values for unsupplied fields
      clear lt_message.
      read_single(
        exporting
          iv_addr_number = cs_addr_val-addrnumber       " Address number
        importing
          et_message     = lt_message
        receiving
          rs_addr_val    = data(ls_addr_val) ).             " Address transfer structure

      if ls_addr_val is initial.
        append lines of lt_message to et_message.
        unlock_address.
        addr_memory_clear.
        return.
      endif.

      ls_addr_val = corresponding #( base ( ls_addr_val ) cs_addr_val ).

      clear cs_addr_val.
      cs_addr_val = ls_addr_val.
      clear ls_addr_val.

      data(ls_addr_data) = corresponding addr1_data( cs_addr_val ).

      clear:
        lv_rc_addr_update,
        lv_data_changed,
        lt_addr_error.

      call function 'ADDR_UPDATE'
        exporting
          address_data         = ls_addr_data
          address_number       = cs_addr_val-addrnumber             " Address Number (Key of Database Table)
          check_empty_address  = abap_false
          check_address        = abap_false                         " Flag: Check address contents
          check_other_versions = abap_false
*         blk_excpt            = abap_true         " only respected if BUPA_ILM_BF is activated
        importing
          address_data         = ls_addr_data
          returncode           = lv_rc_addr_update
          data_has_changed     = lv_data_changed    " Indicator: Data Has Been Changed
        tables
          error_table          = lt_addr_error      " Table with errors, warnings, information
        exceptions
          address_not_exist    = 1                " Address does not exist
          parameter_error      = 2                " Incorrect parameter values
          version_not_exist    = 3
          internal_error       = 4                " Serious internal error (MESSAGE A...)
          address_blocked      = 5
          others               = 6.
      case sy-subrc.
        when 0.
          if lv_rc_addr_update = 'E'.
            if lt_addr_error is not initial.
              loop at lt_addr_error into data(ls_addr_error).
                clear mv_message.
                message id ls_addr_error-msg_id type ls_addr_error-msg_type number ls_addr_error-msg_number
                  with ls_addr_error-msg_var1 ls_addr_error-msg_var2 ls_addr_error-msg_var3 ls_addr_error-msg_var4 into mv_message.

                append mv_message to et_message.
                clear ls_addr_error.
              endloop.
            endif.
            unlock_address.
            addr_memory_clear. " or addr_memory_clear_single '' lv_addr_handle.
            return.
          else.
            if lv_data_changed = abap_true.
              addr_memory_save_single cs_addr_val-addrnumber abap_true.

              cs_addr_val = corresponding #( ls_addr_val ).
              rv_updated = abap_true.
            endif.
          endif.
        when others.
          add_message.
          unlock_address.
          addr_memory_clear.
          return.
      endcase.
    else.
      append lines of lt_message to et_message.
      addr_memory_clear.
      return.
    endif.

    unlock_address.

    addr_memory_clear.
  endmethod.
ENDCLASS.
