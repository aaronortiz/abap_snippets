*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature

CLASS lcl_abap_csv DEFINITION FOR TESTING. "#AU Risk_Level Harmless
  "#AU Duration   Short

  "  PUBLIC SECTION.
  PRIVATE SECTION.

    METHODS convert_data_to_csv FOR TESTING.
    METHODS convert_csv_to_table FOR TESTING.
    METHODS convert_struct_to_csv FOR TESTING.
    METHODS convert_struct_ref_to_csv FOR TESTING.
    METHODS convert_table_ref_to_csv FOR TESTING.
    METHODS convert_table_to_csv FOR TESTING.
    METHODS create_delimited_string FOR TESTING.
    METHODS enclose_string_in_symbol FOR TESTING.
    METHODS get_header_row FOR TESTING.

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
      "cr_cut TYPE REF TO zcl_abap_csv,
      cv_msg TYPE string,
      cv_exp TYPE string,
      cv_act TYPE string.

    METHODS setup.

ENDCLASS.                    "DELIMITED DEFINITION

*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

CLASS lcl_abap_csv IMPLEMENTATION.

  METHOD convert_csv_to_table.

    TYPES:
      BEGIN OF test_s,
        field1 TYPE c LENGTH 10,
        field2 TYPE i,
        field3 TYPE p LENGTH 10 DECIMALS 2,
      END OF test_s,

      test_tt TYPE STANDARD TABLE OF test_s WITH DEFAULT KEY.

    DATA:
      lt_test TYPE string_table,
      lt_results TYPE test_tt,
      lt_exp     TYPE test_tt,
      ls_exp     TYPE test_s.

    " Given

    " When
    break efarfan.
    CLEAR: lt_test, lt_exp, ls_exp.
    APPEND: '"FIELD1", "FIELD2", "FIELD3"' TO lt_test,
            '"text", 5, 34.23' TO lt_test,
            '"text2", -5, 134.23' TO lt_test.
    zcl_abap_csv=>convert_csv_to_table( EXPORTING csv_string_table = lt_test
                                        CHANGING destination_table = lt_results ).
    ls_exp-field1 = 'text'.
    ls_exp-field2 = 5.
    ls_exp-field3 = '34.23'.
    APPEND ls_exp TO lt_exp.
    ls_exp-field1 = 'text2'.
    ls_exp-field2 = -5.
    ls_exp-field3 = '134.23'.
    APPEND ls_exp TO lt_exp.

    " Then
    cv_msg = 'Tables should equal'.
    cl_aunit_assert=>assert_equals( act = lt_results exp = lt_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_csv_to_table


  METHOD convert_data_to_csv.

    FIELD-SYMBOLS:
      <field> TYPE ANY.

    DATA:
      lr_data TYPE REF TO data.

    " Given

    " When
    CREATE DATA lr_data TYPE string.
    ASSIGN lr_data->* TO <field>.
    <field> = 'Simple test'.
    cv_act = zcl_abap_csv=>convert_data_to_csv( object = lr_data ).

    " Then
    cv_exp = '"Simple test"'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " More complicated tests done individually for other methods called by this one

  ENDMETHOD.                    "convert_data_to_csv


  METHOD convert_struct_to_csv.

    TYPES:
      BEGIN OF test_s,
        field1 TYPE c LENGTH 10,
        field2 TYPE i,
        field3 TYPE p LENGTH 10 DECIMALS 2,
      END OF test_s.

    DATA:
      ls_test   TYPE test_s.

    " Given
    CLEAR: ls_test.
    ls_test-field1 = 'text'.
    ls_test-field2 = 5.
    ls_test-field3 = '34.23'.

    " When
    cv_act = zcl_abap_csv=>convert_struct_to_csv( ls_test ).

    " Then
    cv_exp = '"text", 5, 34.23'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_struct_to_csv


  METHOD convert_struct_ref_to_csv.

    FIELD-SYMBOLS:
      <field> TYPE ANY,
      <simple_struct> TYPE ANY,
      <struct_field> TYPE ANY.",

    DATA:
      lv_simple_string TYPE string,
      ls_simple_struct TYPE simple_struct_s,
      lr_data TYPE REF TO data.

    " Given

    " When
    CREATE DATA lr_data TYPE simple_struct_s.
    ASSIGN lr_data->* TO <simple_struct>.
    IF <simple_struct> IS ASSIGNED.
      ASSIGN COMPONENT 'MEANINGLESS' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'BABBLE'.
      ENDIF.
      ASSIGN COMPONENT 'UNIMPORTANT' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = -7.
      ENDIF.
      ASSIGN COMPONENT 'ARBITRARY' OF STRUCTURE <simple_struct> TO <struct_field>.
      IF <struct_field> IS ASSIGNED.
        <struct_field> = 'X'.
      ENDIF.
    ENDIF.
    cv_act = zcl_abap_csv=>convert_data_to_csv( object = lr_data ).

    " Then
    cv_exp = '"BABBLE", -7, "X"'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_struct_ref_to_csv


  METHOD convert_table_to_csv.

    TYPES:
      BEGIN OF test_s,
        field1 TYPE c LENGTH 10,
        field2 TYPE i,
        field3 TYPE p LENGTH 10 DECIMALS 2,
      END OF test_s,

      test_tt TYPE STANDARD TABLE OF test_s WITH DEFAULT KEY.

    DATA:
      lt_test   TYPE test_tt,
      lt_result TYPE string_table,
      ls_test   TYPE test_s.

    " Given
    CLEAR: lt_test, ls_test.
    ls_test-field1 = 'text'.
    ls_test-field2 = 5.
    ls_test-field3 = '34.23'.
    APPEND ls_test TO lt_test.
    ls_test-field1 = 'text 2'.
    ls_test-field2 = 15.
    ls_test-field3 = '134.23'.
    APPEND ls_test TO lt_test.

    " When
    lt_result = zcl_abap_csv=>convert_table_to_csv( table = lt_test ).

    " Then
    READ TABLE lt_result INDEX 2 INTO cv_act.

    cv_exp = '"text", 5, 34.23'.

    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    READ TABLE lt_result INDEX 3 INTO cv_act.
    cv_exp = '"text 2", 15, 134.23'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "convert_table_to_csv


  METHOD convert_table_ref_to_csv.

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

    " When
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

    cv_act = zcl_abap_csv=>convert_data_to_csv( object = lr_data ).

    " Then
    CONCATENATE '"MEANINGLESS", "UNIMPORTANT", "ARBITRARY"'
                '"BABBLE", 7, "X"'
                '"BABBLE", 7, "X"'
                '"BABBLE", 7, "X"'
    INTO cv_exp SEPARATED BY cl_abap_char_utilities=>cr_lf.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


  ENDMETHOD.                    "convert_table_ref_to_csv


  METHOD create_delimited_string.

    DATA:
      lt_test_table TYPE string_table.

    " Given

    " When
    APPEND 'Tom' TO lt_test_table.
    cv_act = zcl_abap_csv=>create_delimited_string( strings = lt_test_table ).
    " Then
    cv_exp = 'Tom'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    APPEND 'Dick' TO lt_test_table.
    cv_act = zcl_abap_csv=>create_delimited_string( strings = lt_test_table ).

    " Then
    cv_exp = 'Tom, Dick'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    APPEND 'Harry' TO lt_test_table.
    cv_act = zcl_abap_csv=>create_delimited_string( strings = lt_test_table
                                                           custom_separator = `, and ` ).

    " Then
    cv_exp = 'Tom, and Dick, and Harry'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

    " When
    cv_act = zcl_abap_csv=>create_delimited_string( strings = lt_test_table
                                                           add_cr_lf = 'X' ).

    " Then
    CONCATENATE `Tom, ` `Dick, ` `Harry` INTO cv_exp SEPARATED BY cl_abap_char_utilities=>cr_lf.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).


  ENDMETHOD.                    "create_DELIMITED_string


  METHOD enclose_string_in_symbol.

    " Given
    DATA: lv_inner_string TYPE string.

    lv_inner_string = 'loudCoworker'.

    " When
    cv_act = zcl_abap_csv=>enclose_string_in_symbol( opening_symbol = '"'
                                                     string = lv_inner_string
                                                     closing_symbol = '"'
                                                     add_space = space ).
    " Then
    cv_exp = '"loudCoworker"'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "enclose_string_in_symbol


  METHOD get_header_row.

    " Given
    TYPES:
      BEGIN OF test_s,
        eeny TYPE i,
        meeny TYPE c LENGTH 2,
        miny TYPE p LENGTH 10 DECIMALS 2,
        moe TYPE string,
      END OF test_s,
      test_tt TYPE STANDARD TABLE OF test_s WITH DEFAULT KEY.
    DATA:
      lt_test TYPE test_tt.

    " When
    me->cv_act = zcl_abap_csv=>get_header_row( lt_test ).

    " Then
    me->cv_exp = '"EENY", "MEENY", "MINY", "MOE"'.
    CONCATENATE 'Expected "' cv_exp '" not "' cv_act '".' INTO cv_msg.
    cl_aunit_assert=>assert_equals( act = cv_act exp = cv_exp msg = cv_msg ).

  ENDMETHOD.                    "get_header_row


  METHOD setup.

    CLEAR: cv_msg, cv_exp, cv_act.

    "CREATE OBJECT cr_cut.

  ENDMETHOD.                    "setup

ENDCLASS.                    "lcl_abap_csv IMPLEMENTATION

*"* use this source file for any macro definitions you need
*"* in the implementation part of the class


