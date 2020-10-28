*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

define add_message.
  clear lv_message.
  message id &1 type &2 number &3
    with &4 &5 &6 &7 into lv_message.

  add_message( exporting iv_text = conv #( lv_message ) ).

  &8 = get_messages( ).
  return.
end-of-definition.
