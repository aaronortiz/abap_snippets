class ZCL_ABAP_CSV definition
  public
  create public .

*"* public components of class ZCL_ABAP_CSV
*"* do not include other source files here!!!
public section.

  data LINES type STRING_TABLE read-only .

  class-methods CREATE_DELIMITED_STRING
    importing
      !STRINGS type STRING_TABLE
      !CUSTOM_SEPARATOR type STRING default `, `
      !ADD_CR_LF type FLAG default ''
    returning
      value(COMMA_SEPARATED_STRING) type STRING .
  class-methods ENCLOSE_STRING_IN_SYMBOL
    importing
      !OPENING_SYMBOL type CHAR1 default '('
      !STRING type STRING
      !CLOSING_SYMBOL type CHAR1 default ')'
      !ADD_SPACE type FLAG default 'X'
      !ADD_CR_LF type FLAG default ''
    returning
      value(ENCLOSED_STRING) type STRING .
  class-methods CONVERT_DATA_TO_CSV
    importing
      !OBJECT type ref to DATA
      !RECURSION_COUNT type INT4 default 0
    returning
      value(CSV) type STRING .
  class-methods CONVERT_CSV_TO_TABLE
    importing
      !CSV_STRING_TABLE type STRING_TABLE
    changing
      !DESTINATION_TABLE type ANY TABLE .
  class-methods CONVERT_TABLE_TO_CSV
    importing
      !TABLE type ANY TABLE
      !RECURSION_COUNT type INT4 default 0
    returning
      value(CSV_ROWS) type STRING_TABLE
    exceptions
      DEPTH_NOT_SUPPORTED .
  class-methods CONVERT_TABLE_REF_TO_CSV
    importing
      !TABLE type ref to DATA
      !RECURSION_COUNT type INT4 default 0
    returning
      value(CSV) type STRING
    exceptions
      DEPTH_NOT_SUPPORTED .
  class-methods CONVERT_STRUCT_TO_CSV
    importing
      !STRUCT type ANY
      !RECURSION_COUNT type INT4 default 0
    returning
      value(CSV) type STRING .
  class-methods CONVERT_STRUCT_REF_TO_CSV
    importing
      !STRUCT type ref to DATA
      !RECURSION_COUNT type INT4 default 0
    returning
      value(CSV) type STRING .
  class-methods GET_HEADER_ROW
    importing
      !TABLE type TABLE
    returning
      value(HEADER_CSV) type STRING .

*"* dummy include to reduce generation dependencies between
*"* class ZCL_ABAP_CSV and it's users.
*"* touched if any type reference has been changed

*"* protected components of class ZCL_ABAP_JSON
*"* do not include other source files here!!!
protected section.

*"* private components of class ZCL_ABAP_CSV
*"* do not include other source files here!!!
private section.

ENDCLASS.


CLASS ZCL_ABAP_CSV% IMPLEMENTATION.
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

METHOD convert_data_to_csv.

  FIELD-SYMBOLS:
    <object> TYPE ANY.

  DATA:
    lo_typdes  TYPE REF TO cl_abap_typedescr,
    lv_value   TYPE string,
    lv_recursion_count TYPE i.

  CLEAR csv.

  ASSIGN object->* TO <object>.
  lv_recursion_count = recursion_count + 1.

  IF <object> IS ASSIGNED.

    TRY.
        " Use RTTS to check whether object is a variable, structure or table
        lo_typdes ?= cl_abap_typedescr=>describe_by_data( <object> ).

        IF lo_typdes->kind = cl_abap_typedescr=>kind_struct.
          csv = zcl_abap_csv=>convert_struct_ref_to_csv( struct = object
                                                         recursion_count = lv_recursion_count ).

        ELSEIF lo_typdes->kind = cl_abap_typedescr=>kind_table.
          csv = zcl_abap_csv=>convert_table_ref_to_csv( table = object
                                                        recursion_count = lv_recursion_count ).
        ELSE.
          lv_value = <object>. " Convert to string if possible
          CONDENSE lv_value.
          IF lo_typdes->type_kind = cl_abap_typedescr=>typekind_string
          OR lo_typdes->type_kind = cl_abap_typedescr=>typekind_char.

            csv = enclose_string_in_symbol( opening_symbol = '"'
                                             string = lv_value
                                             closing_symbol = '"'
                                             add_space = space ).
          ELSE.
            " Deal with negatives on the right side (oh SAP...)
            IF lv_value CO ' -,.0123456789' AND lv_value CS '-'.
              REPLACE ALL OCCURRENCES OF '-' IN lv_value WITH ''.
              CONCATENATE '-' lv_value INTO lv_value.
              CONDENSE lv_value.
            ENDIF.
            csv = lv_value.

          ENDIF.
        ENDIF.

      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDIF.

ENDMETHOD.

METHOD convert_struct_ref_to_csv.

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
    lv_row_csv          TYPE string,
    lv_recursion_count  type i.

  ASSIGN struct->* TO <object>.

  CLEAR lt_row_strings.
  lv_recursion_count = recursion_count + 1.
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

      " Call method recursively with component and insert into CSV
      lv_row_csv = convert_data_to_csv( object = lr_struct_component
                                        recursion_count = lv_recursion_count ).
      APPEND lv_row_csv TO lt_row_strings.

    ENDIF.
  ENDLOOP.

  IF NOT lt_row_strings[] IS INITIAL. " Insert result of recursive call into CSV object
    csv = create_delimited_string( strings = lt_row_strings ).
  ENDIF.

ENDMETHOD.

METHOD convert_table_ref_to_csv.

  FIELD-SYMBOLS:
    <table> TYPE ANY TABLE,
    <table_row> TYPE ANY,
    <table_row_data> TYPE ANY.

  DATA:
    lo_tdescr          TYPE REF TO cl_abap_tabledescr,
    lr_table_row       TYPE REF TO data,
    lt_row_strings     TYPE string_table,
    lv_row_csv         TYPE string,
    lv_recursion_count TYPE i,
    lv_cr_lf           TYPE string.

  IF recursion_count GT 1.
    RAISE depth_not_supported.
  ENDIF.

  ASSIGN table->* TO <table>.

  IF <table> IS ASSIGNED.

    CLEAR lt_row_strings.

    lv_recursion_count = recursion_count + 1.

    " Add header row
    lv_row_csv = zcl_abap_csv=>get_header_row( <table> ).
    APPEND lv_row_csv TO lt_row_strings.

    LOOP AT <table> ASSIGNING <table_row>.

      " create copy of data and create data reference to it
      CREATE DATA lr_table_row LIKE LINE OF <table>.
      ASSIGN lr_table_row->* TO <table_row_data>.
      IF <table_row_data> IS ASSIGNED.

        <table_row_data> = <table_row>.

        " Call method recursively for each row in the table
        lv_row_csv = convert_data_to_csv( object = lr_table_row
                                          recursion_count = lv_recursion_count ).

        APPEND lv_row_csv TO lt_row_strings.
      ENDIF.
    ENDLOOP.

    IF NOT lt_row_strings[] IS INITIAL.
      lv_cr_lf = cl_abap_char_utilities=>cr_lf.
      csv = create_delimited_string( strings = lt_row_strings
                                     custom_separator = lv_cr_lf ).
    ENDIF.
  ENDIF.

ENDMETHOD.

METHOD CREATE_DELIMITED_STRING.

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

METHOD convert_table_to_csv.

  FIELD-SYMBOLS:
    <table> TYPE ANY TABLE,
    <table_row> TYPE ANY,
    <table_row_data> TYPE ANY.

  DATA:
    lo_tdescr          TYPE REF TO cl_abap_tabledescr,
    lr_table_row       TYPE REF TO data,
    lv_row_csv         TYPE string,
    lv_recursion_count TYPE i,
    lv_cr_lf           TYPE string.

  IF recursion_count GT 1.
    RAISE depth_not_supported.
  ENDIF.

  ASSIGN table TO <table>.

  IF <table> IS ASSIGNED.

    CLEAR csv_rows.

    lv_recursion_count = recursion_count + 1.

    " Add header row
    lv_row_csv = zcl_abap_csv=>get_header_row( table ).
    APPEND lv_row_csv TO csv_rows.

    LOOP AT <table> ASSIGNING <table_row>.

      " create copy of data and create data reference to it
      CREATE DATA lr_table_row LIKE LINE OF <table>.
      ASSIGN lr_table_row->* TO <table_row_data>.
      IF <table_row_data> IS ASSIGNED.

        <table_row_data> = <table_row>.

        " Call method recursively for each row in the table
        lv_row_csv = convert_data_to_csv( object = lr_table_row
                                          recursion_count = lv_recursion_count ).

        APPEND lv_row_csv TO csv_rows.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDMETHOD.

METHOD CONVERT_STRUCT_TO_CSV.

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
    lv_row_csv          TYPE string,
    lv_recursion_count  type i.

  ASSIGN struct TO <object>.

  CLEAR lt_row_strings.
  lv_recursion_count = recursion_count + 1.
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

      " Call method recursively with component and insert into CSV
      lv_row_csv = convert_data_to_csv( object = lr_struct_component
                                        recursion_count = lv_recursion_count ).
      APPEND lv_row_csv TO lt_row_strings.

    ENDIF.
  ENDLOOP.

  IF NOT lt_row_strings[] IS INITIAL. " Insert result of recursive call into CSV object
    csv = create_delimited_string( strings = lt_row_strings ).
  ENDIF.

ENDMETHOD.

METHOD get_header_row.

  FIELD-SYMBOLS:
    <table_row> TYPE ANY.

  DATA:
    lo_sdescr       TYPE REF TO cl_abap_structdescr,
    ls_component    TYPE abap_compdescr,
    lr_table_row    TYPE REF TO data,
    lv_header_name  TYPE string,
    lt_header_names TYPE string_table.

  CLEAR header_csv.

  CREATE DATA lr_table_row LIKE LINE OF table.
  ASSIGN lr_table_row->* TO <table_row>.

  IF sy-subrc EQ 0 AND <table_row> IS ASSIGNED.

    lo_sdescr ?= cl_abap_structdescr=>describe_by_data( <table_row> ).

    LOOP AT lo_sdescr->components INTO ls_component.
      CONCATENATE '"' ls_component-name '"' INTO lv_header_name.
      APPEND lv_header_name TO lt_header_names.
    ENDLOOP.

    IF NOT lt_header_names[] IS INITIAL.
      header_csv = zcl_abap_csv=>create_delimited_string( lt_header_names ).
    ENDIF.
  ENDIF.

ENDMETHOD.

METHOD convert_csv_to_table.

  FIELD-SYMBOLS:
    <new_row>   TYPE ANY,
    <value>     TYPE ANY,
    <new_table> TYPE table.

  DATA:
    lt_row_data  TYPE string_table,
    lr_new_row   TYPE REF TO data,
    lr_new_table TYPE REF TO data,
    lv_line      TYPE string,
    lv_value     TYPE string,
    lv_index     TYPE i,
    lv_offset    TYPE i.

  CREATE DATA: lr_new_row   LIKE LINE OF destination_table,
               lr_new_table LIKE         destination_table.

  ASSIGN: lr_new_row->* TO <new_row>,
          lr_new_table->* TO <new_table>.

  IF <new_row> IS ASSIGNED AND <new_table> IS ASSIGNED.

    LOOP AT csv_string_table FROM 2 INTO lv_line.

      lv_index = 1.
      CLEAR lt_row_data.
      SPLIT lv_line AT ', ' INTO TABLE lt_row_data.

      LOOP AT lt_row_data INTO lv_value.

        ASSIGN COMPONENT lv_index OF STRUCTURE <new_row> TO <value>.
        IF <value> IS ASSIGNED.

          IF lv_value(1) = '"'.
            lv_offset =  STRLEN( lv_value ) - 1.
            IF lv_value+lv_offset(1) = '"'.
              REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH ''.
            ENDIF.
          ENDIF.

          <value> = lv_value.
        ENDIF.
        ADD 1 TO lv_index.

      ENDLOOP.

      APPEND <new_row> TO <new_table>.

    ENDLOOP.

    IF NOT <new_table>[] IS INITIAL.
      destination_table[] = <new_table>[].
    ENDIF.

  ENDIF.

ENDMETHOD.

ENDCLASS.
