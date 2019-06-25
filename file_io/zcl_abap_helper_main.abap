class ZCL_ABAP_HELPER definition
  public
  final
  create public .

*"* public components of class ZCL_ABAP_HELPER
*"* do not include other source files here!!!
public section.

  constants C_OBJECT_CLASS type TROBJTYPE value 'CLAS'. "#EC NOTEXT
  constants C_PGMID_CLASS type PGMID value 'R3TR'. "#EC NOTEXT
  constants C_SUBC_INCLUDE type CHAR1 value 'I'. "#EC NOTEXT

  class-methods SOURCE_DOWNLOAD_CLASS
    importing
      !CLASS_NAME type SOBJ_NAME .

*"* dummy include to reduce generation dependencies between
*"* class ZCL_ABAP_HELPER and it's users.
*"* touched if any type reference has been changed

*"* protected components of class ZCL_ABAP_HELPER
*"* do not include other source files here!!!
protected section.

*"* private components of class ZCL_ABAP_HELPER
*"* do not include other source files here!!!
private section.

ENDCLASS.


CLASS ZCL_ABAP_HELPER% IMPLEMENTATION.
METHOD source_download_class.

  DATA:
    lv_class_name   TYPE sobj_name,
    lr_file_helper  TYPE REF TO zcl_file_helper,

    lt_trdir        TYPE TABLE OF trdir WITH DEFAULT KEY,
    ls_trdir        TYPE          trdir,

    lt_temp         TYPE string_table,
    lt_main_source  TYPE string_table,
    lt_local_source TYPE string_table,
    lv_line         TYPE string.

  CLEAR: lv_class_name, lt_trdir, lt_main_source, lt_local_source.

  CONCATENATE class_name '%' INTO lv_class_name.
  TRANSLATE lv_class_name TO UPPER CASE.

  SELECT * FROM trdir INTO TABLE lt_trdir
  WHERE name LIKE lv_class_name
    AND subc EQ c_subc_include.

  SORT lt_trdir DESCENDING.

  " Loop only through main code
  LOOP AT lt_trdir INTO ls_trdir WHERE name+32(3) EQ '   '.

    CLEAR lt_temp.
    READ REPORT ls_trdir-name INTO lt_temp.

    APPEND LINES OF lt_temp TO lt_main_source.
    APPEND space TO lt_main_source.

  ENDLOOP.

  IF NOT lt_main_source[] IS INITIAL.
    APPEND: 'ENDCLASS.' TO lt_main_source,
            space TO lt_main_source.
    CONCATENATE 'CLASS' lv_class_name 'IMPLEMENTATION.' INTO lv_line SEPARATED BY space.
    APPEND: space TO lt_main_source, lv_line TO lt_main_source.

    " Loop through method and local code
    SORT lt_trdir ASCENDING.
    LOOP AT lt_trdir INTO ls_trdir WHERE name+32(3) NE '   '.

      CLEAR lt_temp.
      READ REPORT ls_trdir-name INTO lt_temp.

      IF ls_trdir-name+30(2) EQ 'CC'.
        APPEND LINES OF lt_temp TO lt_local_source.
        APPEND space TO lt_local_source.
      ELSE.
        APPEND LINES OF lt_temp TO lt_main_source.
        APPEND space TO lt_main_source.
      ENDIF.

    ENDLOOP.

    APPEND 'ENDCLASS.' TO lt_main_source.

    IF NOT lt_main_source[] IS INITIAL.
      CONCATENATE class_name 'main.abap' INTO lv_line SEPARATED BY '_'.
      TRANSLATE lv_line to LOWER CASE.
      CREATE OBJECT lr_file_helper.
      lr_file_helper->write_local_tab_delim( EXPORTING default_filename = lv_line
                                              CHANGING data_to_write = lt_main_source ).
    ENDIF.
    IF NOT lt_local_source[] IS INITIAL.
      CONCATENATE class_name 'local.abap' INTO lv_line SEPARATED BY '_'.
      TRANSLATE lv_line to LOWER CASE.
      lr_file_helper->attributes_clear( ).
      lr_file_helper->write_local_tab_delim( EXPORTING default_filename = lv_line
                                              CHANGING data_to_write = lt_local_source ).
    ENDIF.
  ENDIF.
ENDMETHOD.

ENDCLASS.
