*&---------------------------------------------------------------------*
*& Report ZWRICEF_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZWRICEF_TOOL.

************************************************************************
* WRICEF TOOL
*
* "ABAP Port" (c) Marc Bernard 2021 https://marcbernardtools.com/
*
* REQUIRES SAP GUI 7.70 WITH CHROME BROWSER CONTROL
*
* 2048 game created by Gabriele Cirulli. Based on 1024 by Veewo Studio.
* https://play2048.co/
************************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

DATA:
  html_control TYPE REF TO cl_gui_html_viewer,
  myevent_tab  TYPE cntl_simple_events,
  myevent      TYPE cntl_simple_event,
  ok_code      LIKE sy-ucomm.

CLASS lcl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS on_sapevent
        FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
        postdata.

ENDCLASS.

DATA evt_receiver TYPE REF TO lcl_myevent_handler.

FORM remove_toolbar USING pv_dynnr TYPE sy-dynnr.

  DATA:
    ls_header               TYPE rpy_dyhead,
    lt_containers           TYPE dycatt_tab,
    lt_fields_to_containers TYPE dyfatc_tab,
    lt_flow_logic           TYPE swydyflow.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.

  IF ls_header-no_toolbar = abap_true.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = abap_true.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.

ENDFORM.

FORM output.

  DATA lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute
  APPEND 'SPOS' TO lt_ucomm.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

ENDFORM.

FORM start.
  IF html_control IS INITIAL.

    CREATE OBJECT html_control
      EXPORTING
        parent               = cl_gui_container=>screen0 "my_container
        saphtmlp             = 'X'
        query_table_disabled = 'X'.

    myevent-eventid = html_control->m_id_sapevent.
    myevent-appl_event = 'X'.
    APPEND myevent TO myevent_tab.

    html_control->set_registered_events( myevent_tab ).

    CREATE OBJECT evt_receiver.

    SET HANDLER evt_receiver->on_sapevent FOR html_control.

    PERFORM load_mime_objects.
    PERFORM load_home_page.

  ENDIF.

  CALL SELECTION-SCREEN 1001. " trigger screen
ENDFORM.

FORM exit.

  CASE sy-ucomm.
    WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
      IF NOT html_control IS INITIAL.
        CALL METHOD html_control->free.
        FREE html_control.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.

FORM load_home_page.

  DATA: lv_url TYPE w3url.

  CALL METHOD html_control->load_html_document
    EXPORTING
      document_id  = 'ZWT_INDEX'
    IMPORTING
      assigned_url = lv_url
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc = 0.
    CALL METHOD html_control->show_url
      EXPORTING
        url = lv_url.

  ENDIF.

ENDFORM.

FORM load_mime_objects.

  CALL METHOD html_control->load_mime_object
    EXPORTING
      object_id  = 'ZWT_logo'
      object_url = 'WT_logo.png'
    EXCEPTIONS
      OTHERS     = 1.
  ASSERT sy-subrc = 0.

  CALL METHOD html_control->load_mime_object
    EXPORTING
      object_id  = 'ZWT_INFO'
      object_url = 'WT_info.png'
    EXCEPTIONS
      OTHERS     = 1.
  ASSERT sy-subrc = 0.


  CALL METHOD html_control->load_mime_object
    EXPORTING
      object_id  = 'ZWT_SCRIPT'
      object_url = 'WT_script.js'
    EXCEPTIONS
      OTHERS     = 1.
  ASSERT sy-subrc = 0.

  CALL METHOD html_control->load_mime_object
    EXPORTING
      object_id  = 'ZWT_STYLES'
      object_url = 'WT_styles.css'
    EXCEPTIONS
      OTHERS     = 1.
  ASSERT sy-subrc = 0.

ENDFORM.

CLASS lcl_myevent_handler IMPLEMENTATION.

  METHOD on_sapevent.


  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  PERFORM remove_toolbar USING '1001'.

START-OF-SELECTION.
  PERFORM start.

AT SELECTION-SCREEN OUTPUT.
  PERFORM output.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.
