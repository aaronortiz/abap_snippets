class ZCL_ABAP_JSON definition
  public
  create public .

*"* public components of class ZCL_ABAP_JSON
*"* do not include other source files here!!!
public section.

  class-methods CREATE_COMMA_SEPARATED_STRING
    importing
      !STRINGS type STRING_TABLE
      !CUSTOM_SEPARATOR type STRING default `, `
      !ADD_CR_LF type FLAG default ''
    returning
      value(COMMA_SEPARATED_STRING) type STRING .
  class-methods CREATE_KEY_VALUE_PAIR
    importing
      !KEY type STRING
      !VALUE type STRING
      !OMIT_QUOTES_FOR_VALUE type FLAG default ''
    returning
      value(PAIR) type STRING .
  class-methods ENCLOSE_STRING_IN_SYMBOL
    importing
      !OPENING_SYMBOL type CHAR1 default '('
      !STRING type STRING
      !CLOSING_SYMBOL type CHAR1 default ')'
      !ADD_SPACE type FLAG default 'X'
      !ADD_CR_LF type FLAG default ''
    returning
      value(ENCLOSED_STRING) type STRING .
  type-pools ABAP .
  class-methods CONVERT_DATA_TO_JSON
    importing
      !OBJECT type ref to DATA
      !NAME type ABAP_COMPNAME default ''
      !RECURSION_COUNT type INT4 default 0
    returning
      value(JSON) type STRING .
  class-methods CONVERT_TABLE_TO_JSON
    importing
      !TABLE type ref to DATA
      !NAME type STRING default ''
    returning
      value(JSON) type STRING .
  class-methods CONVERT_STRUCT_TO_JSON
    importing
      !STRUCT type ref to DATA
      !NAME type STRING default ''
    returning
      value(JSON) type STRING .

endclass.

* +-------------------------------------------------------------------------------------------------+
* +-------------------------------------------------------------------------------------------------+

CLASS ZCL_ABAP_JSON IMPLEMENTATION.

  METHOD create_comma_separated_string.

    DATA:
      lv_lines TYPE i,
      lv_count TYPE i,
      lv_separator TYPE string,
      lv_current_line TYPE string.

    DESCRIBE TABLE strings LINES lv_lines.

    CLEAR comma_separated_string.

    IF lv_lines GT 0.

      READ TABLE strings INDEX 1 INTO comma_separated_string.

      IF lv_lines GT 1.

        lv_count = 2.
        IF add_cr_lf EQ space.
          lv_separator = custom_separator.
        ELSE.
          CONCATENATE custom_separator cl_abap_char_utilities=>cr_lf INTO lv_separator.
        ENDIF.

        DO.

          READ TABLE strings INDEX lv_count INTO lv_current_line.

          CONCATENATE comma_separated_string lv_current_line INTO comma_separated_string SEPARATED BY lv_separator.

          IF lv_count GE lv_lines.
            EXIT.
          ELSE.
            ADD 1 TO lv_count.
          ENDIF.

        ENDDO.

      ENDIF.
    ENDIF.

  ENDMETHOD.

* +-------------------------------------------------------------------------------------------------+

  METHOD create_key_value_pair.

    IF omit_quotes_for_value IS INITIAL.
      CONCATENATE '"' key '": "' value '"' INTO pair.
    ELSE.
      CONCATENATE '"' key `": ` value INTO pair.
    ENDIF.

  ENDMETHOD.

* +-------------------------------------------------------------------------------------------------+  

  METHOD enclose_string_in_symbol.

    CLEAR enclosed_string.

    IF NOT add_cr_lf EQ space.
      CONCATENATE opening_symbol
                  cl_abap_char_utilities=>cr_lf
                  string
                  cl_abap_char_utilities=>cr_lf
                  closing_symbol
                  INTO enclosed_string.

    ELSEIF NOT add_space EQ space.

      CONCATENATE opening_symbol
                  ` `
                  string
                  ` `
                  closing_symbol
                  INTO enclosed_string.

    ELSE.

      CONCATENATE opening_symbol
                  string
                  closing_symbol
                  INTO enclosed_string.

    ENDIF.

  ENDMETHOD.

* +-------------------------------------------------------------------------------------------------+

  METHOD convert_data_to_json.

    FIELD-SYMBOLS:
      <object> TYPE ANY.

    DATA:
      lo_typdes           TYPE REF TO cl_abap_typedescr,
      lv_key              TYPE string,
      lv_value            TYPE string.

    CLEAR json.
    lv_key = name.

    ASSIGN object->* TO <object>.

    IF <object> IS ASSIGNED.

      TRY.
          " Use RTTS to check whether object is a variable, structure or table
          lo_typdes ?= cl_abap_typedescr=>describe_by_data( <object> ).

          IF lo_typdes->kind = cl_abap_typedescr=>kind_struct.
            json = convert_struct_to_json( struct = object
                                          name = lv_key ).

          ELSEIF lo_typdes->kind = cl_abap_typedescr=>kind_table.
            json = convert_table_to_json( table = object
                                          name = lv_key ).

          ELSE.
            lv_value = <object>.
            CONDENSE: lv_key, lv_value.
            json = create_key_value_pair( key = lv_key
                                          value = lv_value ).

            IF recursion_count EQ 0.
              json = enclose_string_in_symbol( opening_symbol = '{'
                                              string = json
                                              closing_symbol = '}' ).
            ENDIF.

          ENDIF.

        CATCH cx_sy_move_cast_error.
      ENDTRY.

    ENDIF.

  ENDMETHOD.
  
* +-------------------------------------------------------------------------------------------------+  
  
  METHOD convert_table_to_json.

    FIELD-SYMBOLS:
      <table> TYPE ANY TABLE,
      <table_row> TYPE ANY,
      <table_row_data> TYPE ANY.

    DATA:
      lo_tdescr           TYPE REF TO cl_abap_tabledescr,
      lr_table_row        TYPE REF TO data,
      lt_row_strings      TYPE string_table,
      lv_row_json         TYPE string.

    ASSIGN table->* TO <table>.

    IF <table> IS ASSIGNED.

      CLEAR lt_row_strings.
      LOOP AT <table> ASSIGNING <table_row>.

        " create copy of data and create data reference to it
        CREATE DATA lr_table_row LIKE LINE OF <table>.
        ASSIGN lr_table_row->* TO <table_row_data>.
        IF <table_row_data> IS ASSIGNED.

          <table_row_data> = <table_row>.

          " Call method recursively for each row in the table
          lv_row_json = convert_data_to_json( object = lr_table_row
                                              recursion_count = 1 ).
          APPEND lv_row_json TO lt_row_strings.
        ENDIF.
      ENDLOOP.

      IF NOT lt_row_strings[] IS INITIAL.
        json = create_comma_separated_string( strings = lt_row_strings ).
        json = enclose_string_in_symbol( opening_symbol = '['
                                        string = json
                                        closing_symbol = ']' ).
        IF NOT name IS INITIAL.
          json = create_key_value_pair( key = name
                                        value = json
                                        omit_quotes_for_value = 'X').
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

* +-------------------------------------------------------------------------------------------------+

  METHOD convert_struct_to_json.

    FIELD-SYMBOLS:
      <object> TYPE ANY,
      <struct_component> TYPE ANY,
      <struct_component_data> TYPE ANY.

    DATA:
      lo_ddescr           TYPE REF TO cl_abap_datadescr,
      lo_sdescr           TYPE REF TO cl_abap_structdescr,
      ls_component        TYPE abap_compdescr,
      lr_struct_component TYPE REF TO data,
      lt_row_strings      TYPE string_table,
      lv_row_json         TYPE string.

    ASSIGN struct->* TO <object>.

    CLEAR lt_row_strings.
    lo_sdescr ?= cl_abap_structdescr=>describe_by_data( <object> ).

    LOOP AT lo_sdescr->components INTO ls_component.

      " Call method recursively for every member in structure
      ASSIGN COMPONENT ls_component-name OF STRUCTURE <object> TO <struct_component>.

      IF <struct_component> IS ASSIGNED.
        " create copy of data and create data reference to it
        lo_ddescr ?= cl_abap_datadescr=>describe_by_data( <struct_component> ).
        CREATE DATA lr_struct_component TYPE HANDLE lo_ddescr.
        ASSIGN lr_struct_component->* TO <struct_component_data>.
        <struct_component_data> = <struct_component>.

        " Call method recursively with component and insert into json
        lv_row_json = convert_data_to_json( object = lr_struct_component
                                            name = ls_component-name
                                            recursion_count = 1 ).
        APPEND lv_row_json TO lt_row_strings.

      ENDIF.
    ENDLOOP.

    IF NOT lt_row_strings[] IS INITIAL. " Insert result of recursive call into JSON object
      json = create_comma_separated_string( strings = lt_row_strings ).
      json = enclose_string_in_symbol( opening_symbol = '{'
                                      string = json
                                      closing_symbol = '}' ).
        IF NOT name IS INITIAL.
          json = create_key_value_pair( key = name
                                        value = json
                                        omit_quotes_for_value = 'X').
        ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.