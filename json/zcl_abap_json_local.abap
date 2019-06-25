*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature

CLASS lcl_abap_json DEFINITION FOR TESTING. "#AU Risk_Level Harmless
  "#AU Duration   Short

  "  PUBLIC SECTION.
  PRIVATE SECTION.

    METHODS convert_data_to_json FOR TESTING.
    METHODS convert_struct_to_json FOR TESTING.
    METHODS convert_nested_table_to_json FOR TESTING.
    METHODS convert_table_to_json FOR TESTING.
    METHODS convert_u_snake_to_camel FOR TESTING.
    METHODS create_comma_separated_string FOR TESTING.
    METHODS create_key_value_pair FOR TESTING.
    METHODS enclose_string_in_symbol FOR TESTING.

    TYPES:
      BEGIN OF simple_struct_s,
        meaningless TYPE string,
        unimportant TYPE i,
        arbitrary TYPE flag,
      END OF simple_struct_s,

      simple_table_t TYPE STANDARD TABLE OF simple_struct_s WITH DEFAULT KEY,

      BEGIN OF complex_struct_s,
        ambition TYPE string,
        distraction TYPE i,
        derision TYPE simple_struct_s,
        uglification TYPE simple_table_t,
      END OF complex_struct_s,

      nested_table_t TYPE STANDARD TABLE OF complex_struct_s WITH DEFAULT KEY.

    DATA:
      cr_cut TYPE REF TO zcl_abap_json,
      cv_msg TYPE string,
      cv_exp TYPE string,
      cv_act TYPE string.

    METHODS fill_nested_table
      RETURNING value(nested_table) TYPE nested_table_t.

    METHODS setup.

ENDCLASS.                    "lcl_abap_json DEFINITION

*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

CLASS lcl_abap_json IMPLEMENTATION.

  METHOD convert_data_to_json.

    FIELD-SYMBOLS:
      <field> TYPE ANY.

    DATA:
      lr_data TYPE REF TO data.

    " Given

    " When we have a single key pair
    CREATE DATA lr_data TYPE string.
    ASSIGN lr_data->* TO <field>.
    <field> = 'Simple test'.
    cv_act = zcl_abap_json=>convert_data_to_json( object = lr_data
                                                    name = 'simple' ).

    " Then we get a simple object
    cv_exp = '{ "simple": "Simple test" }'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " More complicated tests done individually for other methods called by this one

  ENDMETHOD.                    "convert_data_to_json

  METHOD convert_struct_to_json.

    FIELD-SYMBOLS:
      <field> TYPE ANY,
      <simple_struct> TYPE ANY,
      <struct_field> TYPE ANY.",

    DATA:
      lv_simple_string TYPE string,
      ls_simple_struct TYPE simple_struct_s,
      lr_data TYPE REF TO data.

    " Given

    " When we have a simple struct
    CREATE DATA lr_data TYPE simple_struct_s.
    ASSIGN lr_data->* TO <simple_struct>.
    IF <simple_struct> IS ASSIGNED.
      ASSIGN COMPONENT 'MEANINGLESS' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'BABBLE'.
      ENDIF.
      ASSIGN COMPONENT 'UNIMPORTANT' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 7.
      ENDIF.
      ASSIGN COMPONENT 'ARBITRARY' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'X'.
      ENDIF.
    ENDIF.
    cv_act = zcl_abap_json=>convert_data_to_json( object = lr_data ).

    " Then the method returns an object
    cv_exp = '{ "meaningless": "BABBLE", "unimportant": "7", "arbitrary": "X" }'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_struct_to_json


  METHOD convert_nested_table_to_json.

    FIELD-SYMBOLS:
      <nested_table> TYPE ANY TABLE.

    DATA:
      lr_nested_table TYPE REF TO data.

    " Given

    " When we fill a complex structure

    CREATE DATA lr_nested_table TYPE nested_table_t.
    ASSIGN lr_nested_table->* TO <nested_table>.

    IF <nested_table> IS ASSIGNED.
      <nested_table>[] = fill_nested_table( ).

      cv_act = zcl_abap_json=>convert_data_to_json( lr_nested_table ).
    ENDIF.

    " Then we get a complex result
    CONCATENATE
'[ { "ambition": "Be Lewis Caroll", "distraction": "11", "derision": { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" }, "uglification": [ { "meaningless": "Life, the universe and everything", "'
'unimportant": "42", "arbitrary": "X" }, { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" }, { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" } ] }, { "ambition": "'
'Be Lewis Caroll", "distraction": "11", "derision": { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" }, "uglification": [ { "meaningless": "Life, the universe and everything", "unimportant": "42", "'
'arbitrary": "X" }, { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" }, { "meaningless": "Life, the universe and everything", "unimportant": "42", "arbitrary": "X" } ] } ]'
    INTO cv_exp.

    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_nested_table_to_json

  METHOD convert_table_to_json.

    FIELD-SYMBOLS:
      <field> TYPE ANY,
      <simple_struct> TYPE ANY,
      <struct_field> TYPE ANY,
      <simple_table> TYPE ANY TABLE,
      <nested_table> TYPE ANY TABLE.

    DATA:
      lv_simple_string TYPE string,
      ls_simple_struct TYPE simple_struct_s,
      lt_simple_table TYPE simple_table_t,
      lt_nested_table TYPE zdw_mission_t,
      lr_data TYPE REF TO data.

    " Given

    " When we send a table
    CREATE DATA lr_data TYPE simple_struct_s.
    ASSIGN lr_data->* TO <simple_struct>.
    IF <simple_struct> IS ASSIGNED.
      ASSIGN COMPONENT 'MEANINGLESS' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'BABBLE'.
      ENDIF.
      ASSIGN COMPONENT 'UNIMPORTANT' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 7.
      ENDIF.
      ASSIGN COMPONENT 'ARBITRARY' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'X'.
      ENDIF.
    ENDIF.

    CLEAR lt_simple_table.
    " Reuses simple structure from earlier test.
    APPEND: <simple_struct> TO lt_simple_table,
            <simple_struct> TO lt_simple_table,
            <simple_struct> TO lt_simple_table.

    CREATE DATA lr_data TYPE simple_table_t.
    ASSIGN lr_data->* TO <simple_table>.

    <simple_table>[] = lt_simple_table[].

    cv_act = zcl_abap_json=>convert_data_to_json( object = lr_data ).

    " Then we get an array
    cv_exp = '[ { "meaningless": "BABBLE", "unimportant": "7", "arbitrary": "X" }, { "meaningless": "BABBLE", "unimportant": "7", "arbitrary": "X" }, { "meaningless": "BABBLE", "unimportant": "7", "arbitrary": "X" } ]'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


  ENDMETHOD.                    "convert_table_to_json


  METHOD convert_u_snake_to_camel.

    " Given

    " When simple case
    cv_act = zcl_abap_json=>convert_u_snake_to_camel('WHATCHU_LOOKING_AT').

    " Then simple result
    cv_exp = 'whatchuLookingAt'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


    " When there are underscores at the start or end
    cv_act = zcl_abap_json=>convert_u_snake_to_camel('_WHATCHU__LOOKING_AT_').

    " Then the method will remove them
    cv_exp = 'whatchuLookingAt'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


    " When there is a double underscore at the beginning
    cv_act = zcl_abap_json=>convert_u_snake_to_camel('__WHATCHU__LOOKING_AT_').

    " Then the method will remove it without setting the 3rd char to uppercase
    cv_exp = 'whatchuLookingAt'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


    " When there are already lowercase chars
    cv_act = zcl_abap_json=>convert_u_snake_to_camel('whatchuLookingAt').

    " Then the method will respect them
    cv_exp = 'whatchuLookingAt'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).



  ENDMETHOD.                    "convert_u_snake_to_camel


  METHOD create_comma_separated_string.

    DATA:
      lt_test_table TYPE string_table.

    " Given

    " When
    APPEND 'Tom' TO lt_test_table.
    cv_act = zcl_abap_json=>create_comma_separated_string( strings = lt_test_table ).
    " Then
    cv_exp = 'Tom'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    APPEND 'Dick' TO lt_test_table.
    cv_act = zcl_abap_json=>create_comma_separated_string( strings = lt_test_table ).

    " Then
    cv_exp = 'Tom, Dick'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    APPEND 'Harry' TO lt_test_table.
    cv_act = zcl_abap_json=>create_comma_separated_string( strings = lt_test_table
                                                           custom_separator = `, and ` ).

    " Then
    cv_exp = 'Tom, and Dick, and Harry'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    cv_act = zcl_abap_json=>create_comma_separated_string( strings = lt_test_table
                                                           add_cr_lf = 'X' ).

    " Then
    CONCATENATE `Tom, ` `Dick, ` `Harry` INTO cv_exp SEPARATED BY cl_abap_char_utilities=>cr_lf.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


  ENDMETHOD.                    "create_comma_separated_string

  METHOD create_key_value_pair.

    " Given
    cv_exp = '"annoyingVoice": "loudCoworker"'.

    " When
    cv_act = zcl_abap_json=>create_key_value_pair( key = 'annoyingVoice'
                                                   value = 'loudCoworker' ).

    " Then
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "create_key_value_pair

  METHOD enclose_string_in_symbol.

    " Given
    DATA: lv_inner_string TYPE string.

    lv_inner_string = zcl_abap_json=>create_key_value_pair( key = 'annoyingVoice'
                                                            value = 'loudCoworker' ).

*------------------------------------------------------------------------------

    " When
    cv_act = zcl_abap_json=>enclose_string_in_symbol( opening_symbol = '{'
                                                      string = lv_inner_string
                                                      closing_symbol = '}' ).

    " Then
    cv_exp = '{ "annoyingVoice": "loudCoworker" }'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

*------------------------------------------------------------------------------

    " When
    cv_act = zcl_abap_json=>enclose_string_in_symbol( opening_symbol = '{'
                                                      string = lv_inner_string
                                                      closing_symbol = '}'
                                                      add_space = space ).
    " Then
    cv_exp = '{"annoyingVoice": "loudCoworker"}'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

*------------------------------------------------------------------------------

    " When
    cv_act = zcl_abap_json=>enclose_string_in_symbol( opening_symbol = '{'
                                                      string = lv_inner_string
                                                      closing_symbol = '}'
                                                      add_cr_lf = 'X' ).
    " Then
    CONCATENATE '{' cl_abap_char_utilities=>cr_lf
                '"annoyingVoice": "loudCoworker"'
                cl_abap_char_utilities=>cr_lf '}' INTO cv_exp.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "enclose_string_in_symbol

  METHOD fill_nested_table.

    DATA:
      ls_complex_struct TYPE complex_struct_s,
      ls_simple_struct TYPE simple_struct_s.

    CLEAR: nested_table, ls_complex_struct, ls_simple_struct.

    ls_simple_struct-meaningless = 'Life, the universe and everything'.
    ls_simple_struct-unimportant = 42.
    ls_simple_struct-arbitrary = 'X'.

    ls_complex_struct-ambition = 'Be Lewis Caroll'.
    ls_complex_struct-distraction = 11.
    ls_complex_struct-derision = ls_simple_struct.

    DO 3 TIMES.
      APPEND ls_simple_struct TO ls_complex_struct-uglification.
    ENDDO.

    DO 2 TIMES.
      APPEND ls_complex_struct TO nested_table.
    ENDDO.

  ENDMETHOD.                    "fill_nested_table

  METHOD setup.

    CLEAR: cv_msg, cv_exp, cv_act.

  ENDMETHOD.                    "setup

ENDCLASS.                    "lcl_abap_json IMPLEMENTATION

*"* use this source file for any macro definitions you need
*"* in the implementation part of the class


