*&H*********************************************************************
*&H      Developer     : Hamad Ahmad                                   *
*&H      Date          : 22.09.2023                                    *
*&H      Company       :                                               *
*&H      For           : Public                        .               *
*&P*********************************************************************
*&D             Report : ZWRICEF_TOOL                                  *
*&D Program Defination : Find the SAP Objects                          *
*&A*********************************************************************
*&A PROGRAM CHANGES / Modification Logs :                              *
*&A +-----------------------------------------------------------------+*
*&A I Date  I    Code          I Programmer    I     Changes          I*
*&A +-----------------------------------------------------------------+*
*&A         I                  I               I                       *
*&A*********************************************************************
* SAHK900100       HAMMAD     TR: For testing Abap Objects [HMD]
************************************************************************

"CL_REST_RESOURCE // SuperClass
CLASS zcl_new_gttab_rp DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_result,
        obj_name TYPE char100,
        ddtext   TYPE char100,
      END OF t_result .

    DATA:
      it_result TYPE STANDARD TABLE OF t_result .
    TYPES: t_stoken          TYPE STANDARD TABLE OF stokex,
           t_overflow(30000) TYPE c.
    DATA:  wa_result TYPE t_result.

* Work Areas: ABAP Workbench
    DATA: BEGIN OF wa_d010inc.
    DATA: master TYPE d010inc-master.
    DATA: END OF wa_d010inc.

    DATA: BEGIN OF wa_tfdir.
    DATA: funcname TYPE tfdir-funcname,
          pname    TYPE tfdir-pname,
          include  TYPE tfdir-include.
    DATA: END OF wa_tfdir.

    DATA: BEGIN OF wa_tadir.
    DATA: devclass TYPE tadir-devclass.
    DATA: END OF wa_tadir.

    DATA: BEGIN OF wa_tstc.
    DATA: pgmna TYPE tstc-pgmna.
    DATA: END OF wa_tstc.

    DATA: BEGIN OF wa_tstcp.
    DATA: param TYPE tstcp-param.
    DATA: END OF wa_tstcp.

    DATA: BEGIN OF wa_enlfdir.
    DATA: area TYPE enlfdir-area.
    DATA: END OF wa_enlfdir.

* Work Areas: BADIs
    DATA: BEGIN OF wa_sxs_attr.
    DATA: exit_name TYPE sxs_attr-exit_name.
    DATA: END OF wa_sxs_attr.

    DATA: BEGIN OF wa_sxs_attrt.
    DATA: text TYPE sxs_attrt-text.
    DATA: END OF wa_sxs_attrt.

* Work Areas: Enhancements

    DATA: BEGIN OF wa_modsap.
    DATA: member TYPE modsap-member.
    DATA: END OF wa_modsap.

    DATA: BEGIN OF wa_modsapa.
    DATA: name TYPE modsapa-name.
    DATA: END OF wa_modsapa.

    DATA: BEGIN OF wa_modsapt.
    DATA: modtext TYPE modsapt-modtext.
    DATA: END OF wa_modsapt.

* user-exits
    TYPES: BEGIN OF ty_mod,
             member TYPE modact-member,
             name   TYPE modact-name,
             status TYPE modattr-status,
             anam   TYPE modattr-anam,
             adat   TYPE modattr-adat,
           END OF ty_mod.
    DATA:   w_mod  TYPE ty_mod.

    TYPES: BEGIN OF t_userexit,
             type(12)    TYPE c,
             pname       TYPE trdir-name,
             txt(300),
             level(1)    TYPE c,
             modname(30) TYPE c,
             modtext(60) TYPE c,
             modattr     TYPE ty_mod,
             colour(4)   TYPE c,
           END OF t_userexit.

    DATA: it_userexit TYPE STANDARD TABLE OF t_userexit,
          i_userexit  LIKE LINE OF it_userexit.

* Function module developmnet classes
    TYPES: BEGIN OF t_devclass,
             clas TYPE trdir-clas,
           END OF t_devclass.
    DATA: it_devclass TYPE STANDARD TABLE OF t_devclass,
          i_devclass  LIKE LINE OF it_devclass.

* Submit programs
    TYPES: BEGIN OF t_submit,
             pname    TYPE trdir-name,
             level(1),
             done(1),
           END OF t_submit.

    DATA: it_submit TYPE STANDARD TABLE OF t_submit,
          i_submit  LIKE LINE OF it_submit.

* function modules within program
    TYPES: BEGIN OF t_fmodule,
             name     TYPE rs38l-name,
             pname    TYPE trdir-name,
             pname2   TYPE trdir-name,
             level(1),
             bapi(1),
             done(1),
           END OF t_fmodule.
    DATA: it_fmodule TYPE STANDARD TABLE OF t_fmodule,
          i_fmodule  LIKE LINE OF it_fmodule.

    METHODS get_objct_data
      IMPORTING
        !tcode   TYPE sobj_name OPTIONAL
        !pname   TYPE sobj_name OPTIONAL
        !objtype TYPE char3 OPTIONAL
      EXPORTING
        !result  TYPE string .
    METHODS get_objct_cds
      IMPORTING
        !field1 TYPE dd03l-fieldname
        !field2 TYPE dd03l-fieldname OPTIONAL
        !field3 TYPE dd03l-fieldname OPTIONAL
        !field4 TYPE dd03l-fieldname OPTIONAL
      EXPORTING
        !result TYPE string .
    METHODS get_objct_tabl
      IMPORTING
        !field1 TYPE dd03l-fieldname
        !field2 TYPE dd03l-fieldname OPTIONAL
        !field3 TYPE dd03l-fieldname OPTIONAL
        !field4 TYPE dd03l-fieldname OPTIONAL
      EXPORTING
        !result TYPE string.

    METHODS data_search
      IMPORTING
        !p_level   TYPE c
        !l_prog    TYPE trdir-name
        !l_incl    TYPE trdir-name
        c_overflow TYPE t_overflow OPTIONAL
        i_stoken   TYPE t_stoken
        p_pname    TYPE sobj_name
        !objtype   TYPE char3.

    METHODS if_rest_resource~get
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_NEW_GTTAB_RP IMPLEMENTATION.


  METHOD data_search.

    DATA:
      w_funcname TYPE tfdir-funcname,
      w_str(50)  TYPE c,
      wa_stoken  LIKE LINE OF i_stoken,
      w_stoken   LIKE LINE OF i_stoken,
      w_off      TYPE i,
      tabix      LIKE sy-tabix,
      w_index    LIKE sy-tabix,
      lv_object  TYPE tobjt-object.

* Work Areas: Business Transaction Events
    DATA: BEGIN OF wa_tbe01t.
    DATA: text1 TYPE tbe01t-text1.
    DATA: END OF wa_tbe01t.

    DATA: BEGIN OF wa_tps01t.
    DATA: text1 TYPE tps01t-text1.
    DATA: END OF wa_tps01t.

    LOOP AT i_stoken INTO wa_stoken.

      CLEAR i_userexit.

* Workflow
      IF objtype = 'W'.
        IF p_level EQ '1'.    " do not perform for function modules (2nd pass)
          IF wa_stoken-str+1(16) CS 'SWE_EVENT_CREATE'.
            REPLACE ALL OCCURRENCES OF '''' IN wa_stoken-str WITH ''.
            i_userexit-type = 'WorkFlow'.
            i_userexit-txt  = wa_stoken-str.
            CONCATENATE l_prog '/' l_incl INTO i_userexit-pname.
            APPEND i_userexit TO it_userexit.
          ENDIF.
        ENDIF.
      ENDIF.

      tabix = sy-tabix + 1.
      i_userexit-level = p_level.
      IF i_userexit-level = '0'.
        IF l_incl IS INITIAL.
          i_userexit-pname = p_pname.
        ELSE.
          CONCATENATE  p_pname '-' l_incl INTO i_userexit-pname.
        ENDIF.
      ELSE.
        IF l_incl IS INITIAL.
          i_userexit-pname = l_prog.
        ELSE.
          CONCATENATE  l_prog '-' l_incl INTO i_userexit-pname.
        ENDIF.
      ENDIF.

* AUTHORITY-CHECKS
      IF objtype = 'A'.
        CLEAR lv_object.
        IF wa_stoken-str EQ 'AUTHORITY-CHECK'.
          CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
          w_index = sy-tabix + 2.
          READ TABLE i_stoken INDEX w_index INTO w_stoken.
          CHECK NOT w_stoken-str CS 'STRUCTURE'.
          CHECK NOT w_stoken-str CS 'SYMBOL'.
          READ TABLE it_submit WITH KEY pname = w_stoken-str INTO i_submit.
          IF sy-subrc <> 0.
            i_userexit-pname = i_submit-pname.
            i_userexit-type = 'AuthCheck'.
            i_userexit-txt  = w_stoken-str.
            REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH space.
            lv_object = i_userexit-txt.
            SELECT SINGLE * FROM tobjt WHERE object = @lv_object
                                         AND langu  = @sy-langu
                                         INTO @DATA(tobjt).
            i_userexit-modname = 'AUTHORITY-CHECK'.
            i_userexit-modtext = tobjt-ttext.
            APPEND i_userexit TO it_userexit.
            CLEAR tobjt.
          ENDIF.
        ENDIF.
      ENDIF.

* Text searches
*    IF NOT p_text IS INITIAL.
*      IF p_stoken-str CS p_text.
*        i_userexit-pname = i_submit-pname.
*        i_userexit-type = 'TextSearch'.
*        i_userexit-txt  = wa_stoken-str.
*        i_userexit-modname = 'Text Search'.
*        i_userexit-modtext = p_stoken-str.
*        APPEND i_userexit.
*      ENDIF.
*    ENDIF.

* Include (SE38)
      IF wa_stoken-str EQ 'INCLUDE'.
        CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
        w_index = sy-tabix + 1.
        READ TABLE i_stoken INDEX w_index INTO w_stoken.
        CHECK NOT w_stoken-str CS 'STRUCTURE'.
        CHECK NOT w_stoken-str CS 'SYMBOL'.
        READ TABLE it_submit WITH KEY pname = w_stoken-str INTO i_submit.
        IF sy-subrc <> 0.
          i_submit-pname = w_stoken-str.
          i_submit-level = p_level.
          APPEND i_submit TO it_submit.
        ENDIF.
      ENDIF.

* Enhancements (SMOD)
      IF objtype = 'U'.
        IF wa_stoken-str EQ 'CUSTOMER-FUNCTION'.
          CLEAR w_funcname.
          READ TABLE i_stoken INDEX tabix INTO wa_stoken.
          TRANSLATE wa_stoken-str USING ''' '.
          CONDENSE wa_stoken-str.
          IF l_prog IS INITIAL.
            CONCATENATE 'EXIT' p_pname wa_stoken-str INTO w_funcname
                         SEPARATED BY '_'.
          ELSE.
            CONCATENATE 'EXIT' l_prog wa_stoken-str INTO w_funcname
                   SEPARATED BY '_'.
          ENDIF.
          SELECT SINGLE member FROM modsap INTO wa_modsap-member
                WHERE member = w_funcname.
          IF sy-subrc = 0.   " check for valid enhancement
            i_userexit-type = 'Enhancement'.
            i_userexit-txt  = w_funcname.
            APPEND i_userexit TO it_userexit.
          ELSE.
            CLEAR wa_d010inc.
            SELECT SINGLE master INTO wa_d010inc-master
                  FROM d010inc
                     WHERE include = l_prog.
            CONCATENATE 'EXIT' wa_d010inc-master wa_stoken-str INTO w_funcname
                   SEPARATED BY '_'.
            i_userexit-type = 'Enhancement'.
            i_userexit-txt  = w_funcname.
          ENDIF.
        ENDIF.
      ENDIF.

* BADIs (SE18)
      IF objtype = 'B' OR objtype EQ 'C'.
        IF wa_stoken-str CS 'cl_exithandler='.
          w_index = sy-tabix + 4.
          READ TABLE i_stoken INDEX w_index INTO w_stoken.
          i_userexit-txt = w_stoken-str.
          REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH space.
          i_userexit-type = 'BADI'.

          IF objtype = 'C'.   "customer BADIs only
            SELECT SINGLE * FROM sxs_attr WHERE exit_name = @i_userexit-txt
                                            AND internal <> 'X'
                                            INTO @DATA(sxs_attr). " ensure a real BADI
            IF sy-subrc = 0.
              APPEND i_userexit TO it_userexit.
            ENDIF.
          ELSE.
            SELECT SINGLE * FROM sxs_attr WHERE exit_name = @i_userexit-txt INTO @sxs_attr.
            IF sy-subrc = 0.
              APPEND i_userexit TO it_userexit.
            ENDIF.
          ENDIF.
          CLEAR sxs_attr.
        ENDIF.
      ENDIF.

* Business transaction events (FIBF)
      IF objtype = 'E'.
        IF wa_stoken-str CS 'OPEN_FI_PERFORM'.
          i_userexit-type = 'BusTrEvent'.
          i_userexit-txt = wa_stoken-str.
          REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH space.
          i_userexit-modname =  i_userexit-txt+16(8).
          CASE i_userexit-txt+25(1).
            WHEN 'E'.
              CLEAR wa_tbe01t.
              SELECT SINGLE text1 INTO wa_tbe01t-text1 FROM tbe01t
                               WHERE event = i_userexit-txt+16(8)
                                 AND spras = sy-langu.
              IF wa_tbe01t-text1 IS INITIAL.
                i_userexit-modtext = '<Not active>'.        "#EC NOTEXT
              ELSE.
                i_userexit-modtext = wa_tbe01t-text1.
              ENDIF.
              i_userexit-modname+8 = '/P&S'.                "#EC NOTEXT
            WHEN 'P'.
              CLEAR wa_tps01t.
              SELECT SINGLE text1 INTO wa_tps01t-text1 FROM tps01t
                               WHERE procs = i_userexit-txt+16(8)
                                 AND spras = sy-langu.
              i_userexit-modtext = wa_tps01t-text1.
              i_userexit-modname+8 = '/Process'.
          ENDCASE.

          APPEND i_userexit TO it_userexit.
        ENDIF.
      ENDIF.

* Program exits (SE38)
      IF objtype = 'P'.
        IF wa_stoken-str CS 'USEREXIT_'.
          CHECK NOT wa_stoken-str CS '-'.   " ensure not USEREXIT_XX-XXX
          CHECK NOT wa_stoken-str CS '('.   " ensure not SUBMIT_XX(X)
          i_userexit-type = 'Program Exit'.
          i_userexit-txt = wa_stoken-str.
          REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH space.
          APPEND i_userexit TO it_userexit.
          CLEAR i_userexit.
        ENDIF.
      ENDIF.

* Submit programs (SE38)
      IF wa_stoken-str CS 'SUBMIT'.
        CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
        CHECK NOT wa_stoken-str CS '_'.   " ensure not SUBMIT_XXX
        w_index = sy-tabix + 1.
        READ TABLE i_stoken INDEX w_index INTO w_stoken.
        CHECK NOT w_stoken-str CS '_'.   " ensure not SUBMIT_XXX
        REPLACE ALL OCCURRENCES OF '''' IN w_stoken-str WITH space.
        READ TABLE it_submit WITH KEY pname = w_stoken-str INTO i_submit.
        IF sy-subrc <> 0.
          i_submit-pname = w_stoken-str.
          i_submit-level = p_level.
          APPEND i_submit TO it_submit.
        ENDIF.
      ENDIF.

* Perform routines (which reference external programs)
      IF wa_stoken-str CS 'PERFORM'.
        CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
        w_index = sy-tabix + 1.
        READ TABLE i_stoken INDEX w_index INTO w_stoken.
        IF NOT w_stoken-ovfl IS INITIAL.
          w_off = w_stoken-off1 + 10.
          w_str = c_overflow+w_off(30).
          FIND ')' IN w_str MATCH OFFSET w_off.
          IF sy-subrc = 0.
            w_off = w_off + 1.
            w_stoken-str = w_str(w_off).
          ENDIF.
        ENDIF.

        CHECK w_stoken-str CS '('.
        w_off = 0.
        WHILE sy-subrc  = 0.
          IF w_stoken-str+w_off(1) EQ '('.
            REPLACE SECTION OFFSET w_off LENGTH 1 OF w_stoken-str WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN w_stoken-str WITH space.
            READ TABLE it_submit WITH KEY pname = w_stoken-str INTO i_submit.
            IF sy-subrc <> 0.
              i_submit-pname = w_stoken-str.
              APPEND i_submit TO it_submit.
            ENDIF.
            EXIT.
          ELSE.
            REPLACE SECTION OFFSET w_off LENGTH 1 OF w_stoken-str WITH ''.
            SHIFT w_stoken-str LEFT DELETING LEADING space.
          ENDIF.
        ENDWHILE.
      ENDIF.

* Function modules (SE37)
      IF wa_stoken-str CS 'FUNCTION' AND ( objtype EQ 'F' OR objtype EQ 'I' OR objtype EQ 'D' ).

        CLEAR i_fmodule.
        IF p_level EQ '0'.    " do not perform for function modules (2nd pass)
          w_index = sy-tabix + 1.
          READ TABLE i_stoken INDEX w_index INTO w_stoken.

          IF w_stoken-str CS 'BAPI'.
            i_fmodule-bapi = 'X'.
          ENDIF.

          REPLACE FIRST OCCURRENCE OF '''' IN w_stoken-str WITH space.
          REPLACE FIRST OCCURRENCE OF '''' IN w_stoken-str WITH space.
          IF sy-subrc = 4.   " didn't find 2nd quote (ie name truncated)
            CLEAR wa_tfdir.
            CONCATENATE w_stoken-str '%' INTO w_stoken-str.
            SELECT SINGLE funcname INTO wa_tfdir-funcname FROM tfdir
                         WHERE funcname LIKE w_stoken-str.
            IF sy-subrc = 0.
              i_fmodule-name = wa_tfdir-funcname.
            ELSE.
              CONTINUE.
            ENDIF.
          ELSE.
            i_fmodule-name = w_stoken-str.
          ENDIF.
          i_fmodule-level = p_level.
          APPEND i_fmodule TO it_fmodule.
          CLEAR i_fmodule.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_objct_cds.

    DATA:dref TYPE REF TO data.
    FIELD-SYMBOLS:<table> TYPE STANDARD TABLE.
    TYPE-POOLS: slis, icon.

    FIELD-SYMBOLS:<fs_tab13> TYPE mandt.
    DATA:wa_dd1      TYPE dd03l,
         wa_dd2      TYPE dd03l,
         wa_dd3      TYPE dd03l,
         wa_dd4      TYPE dd03l,
         lv_tab      TYPE dd03l-tabname,
         wa_dd_final TYPE dd03l,
         s_exc       TYPE boole-boole VALUE '/',
         s_field1    TYPE dd03l-fieldname,
         s_field2    TYPE dd03l-fieldname,
         s_field3    TYPE dd03l-fieldname,
         s_field4    TYPE dd03l-fieldname.

    s_field1  =  field1.
    s_field2  =  field2.
    s_field3  =  field3.
    s_field4  =  field4.

    IF s_exc IS NOT INITIAL.
      lv_tab = '/%'.
    ENDIF.

    IF s_field2 IS INITIAL.
      s_field2 = s_field1.
    ENDIF.

* Internal Tables
    TYPES: BEGIN OF t_ialv,
             tabname    TYPE dd03l-tabname,
             ddtext     TYPE dd02t-ddtext,
             ddlname    TYPE ddldependency-ddlname,
             objectname TYPE ddldependency-objectname,
             tbtext     TYPE ddddlsrct-ddtext,
             field1     TYPE dd03l-fieldname,
             field2     TYPE dd03l-fieldname,
             field3     TYPE dd03l-fieldname,
             field4     TYPE dd03l-fieldname,
             count(1)   TYPE c,
           END OF t_ialv .

    DATA: ialv   TYPE STANDARD TABLE OF t_ialv,
          wa_alv TYPE t_ialv.

    IF s_field1 IS NOT INITIAL.

      SELECT c~ddlname, c~objectname, p~tabname, p~fieldname, p~rollname
        FROM ( ddldependency AS c
             INNER JOIN dd03l AS p ON p~tabname  = c~objectname
                                  AND p~as4local = c~state )

        WHERE p~fieldname = @s_field1
        AND p~tabname NOT LIKE @lv_tab

           INTO TABLE @DATA(itab13).
    ENDIF.
    IF s_field2 IS NOT INITIAL.

      SELECT c~ddlname, c~objectname, p~tabname, p~fieldname, p~rollname
      FROM ( ddldependency AS c
           INNER JOIN dd03l AS p ON p~tabname  = c~objectname
                                AND p~as4local = c~state )
*                              AND p~cityto   = @cityto )
*         INNER JOIN dd09l AS f ON f~tabname = p~tabname )
      WHERE p~fieldname = @s_field2
        AND p~tabname NOT LIKE @lv_tab
         INTO TABLE @DATA(itab14).
    ENDIF.
    IF s_field3 IS NOT INITIAL.

      SELECT c~ddlname, c~objectname, p~tabname, p~fieldname, p~rollname
      FROM ( ddldependency AS c
           INNER JOIN dd03l AS p ON p~tabname  = c~objectname
                                AND p~as4local = c~state )
      WHERE p~fieldname = @s_field3
        AND p~tabname NOT LIKE @lv_tab
         INTO TABLE @DATA(itab15).
    ENDIF.
    IF s_field4 IS NOT INITIAL.

      SELECT c~ddlname, c~objectname, p~tabname, p~fieldname, p~rollname
      FROM ( ddldependency AS c
           INNER JOIN dd03l AS p ON p~tabname  = c~objectname
                                AND p~as4local = c~state )
*                              AND p~cityto   = @cityto )
*         INNER JOIN dd09l AS f ON f~tabname = p~tabname )
      WHERE p~fieldname = @s_field4
        AND p~tabname NOT LIKE @lv_tab
         INTO TABLE @DATA(itab16).
    ENDIF.
    SORT itab13 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab13 COMPARING tabname..
    SORT itab14 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab14 COMPARING tabname..
    SORT itab15 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab15 COMPARING tabname..
    SORT itab16 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab16 COMPARING tabname.
    IF s_field1 IS NOT INITIAL.
      LOOP AT itab13 INTO DATA(wa_tab133).
        IF itab14[] IS NOT INITIAL.
          READ TABLE itab14  INTO DATA(wa_tab134) WITH KEY ddlname = wa_tab133-ddlname.
          IF sy-subrc = 0.
            IF itab15[] IS NOT INITIAL.
              READ TABLE itab15  INTO DATA(wa_tab135) WITH KEY ddlname = wa_tab133-ddlname."tabname = wa_tab133-tabname.
              IF sy-subrc = 0.
                IF itab16[] IS NOT INITIAL.
                  READ TABLE itab16  INTO DATA(wa_tab136) WITH KEY ddlname = wa_tab133-ddlname."tabname = wa_tab133-tabname.
                  IF sy-subrc = 0.
                    wa_alv-tabname =  wa_tab134-tabname.
                    wa_alv-objectname =  wa_tab134-objectname.
                    wa_alv-ddlname =  wa_tab134-ddlname.
                    wa_alv-field1 =  |?{ s_field1 }|.
                    wa_alv-field2 =  |?{ s_field2 }|.
                    wa_alv-field3 =  |?{ s_field3 }|.
                    wa_alv-field4 =  |?{ s_field4 }|.
                    wa_alv-count  =  '4'.
                    APPEND wa_alv TO ialv.
                    CLEAR wa_alv.
                  ELSE.

                    wa_alv-tabname =  wa_tab134-tabname.
                    wa_alv-objectname =  wa_tab134-objectname.
                    wa_alv-ddlname =  wa_tab134-ddlname.
                    wa_alv-field1 =  |?{ s_field1 }|.
                    wa_alv-field2 =  |?{ s_field2 }|.
                    wa_alv-field3 =  |?{ s_field3 }|.
                    wa_alv-count  =  '3'.
                    APPEND wa_alv TO ialv.
                    CLEAR wa_alv.
                  ENDIF.
                ELSE.

                  wa_alv-objectname =  wa_tab134-objectname.
                  wa_alv-ddlname =  wa_tab134-ddlname.
                  wa_alv-field1 =  |?{ s_field1 }|.
                  wa_alv-field2 =  |?{ s_field2 }|.
                  wa_alv-field3 =  |?{ s_field3 }|.
                  wa_alv-count  =  '3'.
                  APPEND wa_alv TO ialv.
                  CLEAR wa_alv.
                ENDIF.
              ELSE.

                wa_alv-tabname =  wa_tab134-tabname.
                wa_alv-objectname =  wa_tab134-objectname.
                wa_alv-ddlname =  wa_tab134-ddlname.
                wa_alv-field1 =  |?{ s_field1 }|.
                wa_alv-field2 =  |?{ s_field2 }|.
                wa_alv-count  =  '2'.
                APPEND wa_alv TO ialv.
                CLEAR wa_alv.
              ENDIF.

            ELSE.
              wa_alv-tabname =  wa_tab134-tabname.
              wa_alv-objectname =  wa_tab134-objectname.
              wa_alv-ddlname =  wa_tab134-ddlname.
              wa_alv-field1 =  |?{ s_field1 }|.
              wa_alv-field2 =  |?{ s_field2 }|.
              wa_alv-count  =  '2'.
              APPEND wa_alv TO ialv.
              CLEAR wa_tab134.
              CLEAR wa_alv.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM ialv COMPARING ALL FIELDS.
    SORT ialv DESCENDING BY count.
    CLEAR it_result.
    LOOP AT ialv INTO wa_alv.

      SELECT SINGLE ddtext FROM ddddlsrct INTO wa_alv-ddtext WHERE ddlname EQ wa_alv-ddlname.
      wa_result-obj_name = wa_alv-objectname.
      IF wa_alv-field1 EQ wa_alv-field2.
        CONCATENATE wa_alv-ddtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
         wa_alv-field1 wa_alv-field3 wa_alv-field4
          INTO wa_result-ddtext SEPARATED BY space.
      ELSE.
        CONCATENATE wa_alv-ddtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
        wa_alv-field1 wa_alv-field2 wa_alv-field3 wa_alv-field4
          INTO wa_result-ddtext SEPARATED BY space.
      ENDIF.

      MODIFY ialv FROM wa_alv.
      APPEND wa_result TO it_result.
      CLEAR: wa_alv, wa_result.
    ENDLOOP.

    /ui2/cl_json=>serialize(
      EXPORTING
        data   = it_result             " Data to serialize
      RECEIVING
        r_json = result           " JSON string
    ).

  ENDMETHOD.


  METHOD get_objct_data.

    DATA: w_off     TYPE i,
          p_pname   TYPE sobj_name,
          p_tcode   TYPE sobj_name,
          w_area    TYPE rs38l-area,
          w_include TYPE trdir-name.

    DATA: BEGIN OF wa_tstc.
    DATA: pgmna TYPE tstc-pgmna.
    DATA: END OF wa_tstc.

    DATA: BEGIN OF wa_tstcp.
    DATA: param TYPE tstcp-param.
    DATA: END OF wa_tstcp.

* Source code
    TYPES: BEGIN OF t_sourcetab,                    "#EC * (SLIN lügt!)
             line(500),                             "#EC * (SLIN lügt!)
           END OF t_sourcetab.                      "#EC * (SLIN lügt!)
    DATA: sourcetab TYPE STANDARD TABLE OF t_sourcetab." WITH HEADER LINE.
    DATA c_overflow(30000) TYPE c.

* Description of an ABAP/4 source analysis token
    DATA: i_stoken TYPE STANDARD TABLE OF stokex ."WITH HEADER LINE.
*    DATA wa_stoken LIKE LINE OF i_stoken.

* Description of an ABAP/4 source analysis statement
    DATA: i_sstmnt TYPE STANDARD TABLE OF sstmnt." WITH HEADER LINE. "#EC NEEDED

* keywords for searching ABAP code
    TYPES: BEGIN OF t_keywords,
             word(30),
           END OF t_keywords.
    DATA: keywords    TYPE STANDARD TABLE OF t_keywords, " WITH HEADER LINE.
          wa_keywords LIKE LINE OF keywords.

* determine search words

    CASE objtype.
      WHEN 'T'.
        wa_keywords-word = 'SELECT'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'UPDATE'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'INSERT'.
        APPEND wa_keywords TO keywords.
      WHEN OTHERS.
* determine search words
        wa_keywords-word = 'CALL'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'FORM'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'PERFORM'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'SUBMIT'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'INCLUDE'.
        APPEND wa_keywords TO keywords.
        wa_keywords-word = 'AUTHORITY-CHECK'.
        APPEND wa_keywords TO keywords.
    ENDCASE.

    p_tcode = tcode.
    p_pname = pname.

    IF NOT p_tcode IS INITIAL.
* get program name from TCode
      SELECT SINGLE pgmna FROM tstc INTO wa_tstc-pgmna
                   WHERE tcode EQ p_tcode.
      IF NOT wa_tstc-pgmna IS INITIAL.
        p_pname = wa_tstc-pgmna.
* TCode does not include program name, but does have reference TCode
      ELSE.
        SELECT SINGLE param FROM tstcp INTO wa_tstcp-param
                     WHERE tcode EQ p_tcode.
        IF sy-subrc = 0.
          CHECK wa_tstcp-param(1)   = '/'.
          CHECK wa_tstcp-param+1(1) = '*'.
          IF wa_tstcp-param CA ' '.
          ENDIF.
          w_off = sy-fdpos + 1.
          SUBTRACT 2 FROM sy-fdpos.
          IF sy-fdpos GT 0.
            p_tcode = wa_tstcp-param+2(sy-fdpos).
          ENDIF.
          SELECT SINGLE pgmna FROM tstc INTO wa_tstc-pgmna
                 WHERE tcode EQ p_tcode.
          p_pname = wa_tstc-pgmna.
          IF sy-subrc <> 0.
            MESSAGE s110(/saptrx/asc) WITH 'No program found for: ' p_tcode. "#EC NOTEXT
*          STOP.
          ENDIF.
        ELSE.
          MESSAGE s110(/saptrx/asc) WITH 'No program found for: ' p_tcode. "#EC NOTEXT
*        STOP.
        ENDIF.

      ENDIF.
    ENDIF.

* Call customer-function aus Program coding
    READ REPORT p_pname INTO sourcetab.
    IF sy-subrc > 0.
*    MESSAGE e017(enhancement) WITH p_pname RAISING no_program. "#EC *
    ENDIF.

    SCAN ABAP-SOURCE sourcetab    TOKENS     INTO i_stoken
                                  STATEMENTS INTO i_sstmnt
                                  KEYWORDS   FROM keywords
                                  OVERFLOW   INTO c_overflow
                                  WITH INCLUDES WITH ANALYSIS.   "#EC
    IF sy-subrc > 0.
*    MESSAGE e130(enhancement) RAISING syntax_error.         "#EC
    ENDIF.

    IF objtype EQ 'T'.

      DATA(stoken) =  i_stoken[].
      DATA: range_of_tables TYPE RANGE OF obj_name,
            wa_rng          LIKE LINE OF range_of_tables.

      SELECT 'I' AS sign, 'EQ' AS option, str AS low
        FROM @stoken AS stoken
        WHERE str NOT IN ('ITAB' , 'BELNR')
        INTO CORRESPONDING FIELDS OF TABLE @range_of_tables.

*      LOOP AT stoken INTO DATA(wa_stoken) WHERE str ne 'ITAB'.
*
*        wa_rng-sign = 'I'.
*        wa_rng-option = 'EQ'.
*        wa_rng-low = wa_stoken-str.
*
*        APPEND wa_rng TO range_of_tables.
*        CLEAR: wa_rng,wa_stoken.
*
*      ENDLOOP.

      IF NOT range_of_tables IS INITIAL.
        SELECT FROM tadir AS a
          INNER JOIN dd02t AS b ON ( b~tabname EQ a~obj_name AND b~ddlanguage EQ 'E')
           FIELDS
            a~obj_name, b~ddtext
         WHERE object EQ 'TABL'
         AND obj_name IN @range_of_tables
         INTO TABLE @DATA(it_tables).
        SORT it_tables ASCENDING BY obj_name.

      ENDIF.

      /ui2/cl_json=>serialize(
        EXPORTING
          data   = it_tables             " Data to serialize
        RECEIVING
          r_json = result           " JSON string
      ).

    ELSE. "Other Than Tables
      DESCRIBE TABLE i_stoken LINES DATA(w_linnum).
      IF w_linnum GT 0.
        CALL METHOD data_search
          EXPORTING
            p_level    = '0'
            l_prog     = ''
            l_incl     = ''
            c_overflow = c_overflow
            i_stoken   = i_stoken
            p_pname    = p_pname
            objtype    = objtype.
      ENDIF.

      SORT it_submit.
      DELETE ADJACENT DUPLICATES FROM it_submit COMPARING pname.
      DATA(w_level) = '0'.

      LOOP AT it_submit ASSIGNING FIELD-SYMBOL(<fs_submit>) WHERE done NE 'X'.

        CLEAR:    i_sstmnt, sourcetab.
        REFRESH:  i_sstmnt, sourcetab.

        READ REPORT <fs_submit>-pname INTO sourcetab.
        IF sy-subrc = 0.

          SCAN ABAP-SOURCE sourcetab TOKENS     INTO i_stoken
                                     STATEMENTS INTO i_sstmnt
                                     KEYWORDS   FROM keywords
                                     WITH INCLUDES
                                     WITH ANALYSIS.
          IF sy-subrc > 0.
*        message e130(enhancement) raising syntax_error.
            CONTINUE.
          ENDIF.

* check i_stoken for entries
          CLEAR w_linnum.
          DESCRIBE TABLE i_stoken LINES w_linnum.
          IF w_linnum GT 0.
            CALL METHOD data_search
              EXPORTING
                p_level  = w_level
                l_prog   = <fs_submit>-pname
                l_incl   = ''
                i_stoken = i_stoken
                p_pname  = p_pname
                objtype  = objtype.
          ENDIF.
        ENDIF.

* restrict number of submit program selected for processing
        DATA(p_limit) = '500'.
        DESCRIBE TABLE it_submit LINES w_linnum.
        IF w_linnum GE p_limit.
          w_level = '1'.
        ENDIF.
        <fs_submit>-done = 'X'.
      ENDLOOP.

      "Get FM Data

      SORT it_fmodule BY name.
      DELETE ADJACENT DUPLICATES FROM it_fmodule COMPARING name.

      LOOP AT it_fmodule INTO i_fmodule WHERE done NE 'X'.

        CLEAR:   i_stoken, i_sstmnt, sourcetab, wa_tfdir, w_include .
        REFRESH: i_stoken, i_sstmnt, sourcetab.

        CLEAR wa_tfdir.
        SELECT SINGLE funcname pname include FROM tfdir INTO wa_tfdir
                                WHERE funcname = i_fmodule-name.
        CHECK sy-subrc = 0.

        CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
          EXPORTING
            program = wa_tfdir-pname
          IMPORTING
            group   = w_area.

        CONCATENATE 'L' w_area 'U' wa_tfdir-include INTO w_include.
        i_fmodule-pname  = w_include.
        i_fmodule-pname2 = wa_tfdir-pname.
        MODIFY it_fmodule FROM i_fmodule.

        READ REPORT i_fmodule-pname INTO sourcetab.
        IF sy-subrc = 0.

          SCAN ABAP-SOURCE sourcetab TOKENS     INTO i_stoken
                                     STATEMENTS INTO i_sstmnt
                                     KEYWORDS   FROM keywords
                                     WITH INCLUDES
                                     WITH ANALYSIS.
          IF sy-subrc > 0.
*        MESSAGE e130(enhancement) RAISING syntax_error.
          ENDIF.

* check i_stoken for entries
          CLEAR w_linnum.
          DESCRIBE TABLE i_stoken LINES w_linnum.
          IF w_linnum GT 0.
            w_level = '1'.

            CALL METHOD data_search
              EXPORTING
                p_level  = w_level
                l_prog   = i_fmodule-pname2
                l_incl   = i_fmodule-pname
                i_stoken = i_stoken
                p_pname  = p_pname
                objtype  = objtype.

          ENDIF.
        ENDIF.

      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM it_userexit COMPARING txt pname modname.

* store development classes
      IF objtype = 'D'.
        LOOP AT it_fmodule INTO i_fmodule.
          CLEAR: wa_tadir, wa_enlfdir.

          SELECT SINGLE area FROM enlfdir INTO wa_enlfdir-area
                                WHERE funcname = i_fmodule-name.
          CHECK NOT wa_enlfdir-area IS INITIAL.

          SELECT SINGLE devclass INTO wa_tadir-devclass
                          FROM tadir WHERE pgmid    = 'R3TR'
                                       AND object   = 'FUGR'
                                       AND obj_name = wa_enlfdir-area.
          CHECK NOT wa_tadir-devclass IS INITIAL.
          MOVE wa_tadir-devclass TO i_devclass-clas.
          APPEND i_devclass TO it_devclass.
          i_fmodule-done = 'X'.
          MODIFY it_fmodule FROM i_fmodule.
        ENDLOOP.

        SORT it_devclass.
        DELETE ADJACENT DUPLICATES FROM it_devclass.
      ENDIF.

      LOOP AT it_userexit INTO i_userexit.

* Workflow
        IF i_userexit-type EQ 'WorkFlow'.
          CONTINUE.
        ENDIF.

* Enhancement data
        IF  i_userexit-type CS 'Enh'.
          CLEAR: wa_modsapa.
          SELECT SINGLE name INTO wa_modsapa-name FROM modsap
                            WHERE member = i_userexit-txt.
          CHECK sy-subrc = 0.
          i_userexit-modname = wa_modsapa-name.

          CLEAR wa_modsapt.
          SELECT SINGLE modtext INTO wa_modsapt-modtext FROM modsapt
                            WHERE name = wa_modsapa-name
                                         AND sprsl = sy-langu.
          i_userexit-modtext = wa_modsapt-modtext.

* Get the CMOD project name
          CLEAR w_mod.
          SELECT SINGLE modact~member modact~name modattr~status
                        modattr~anam  modattr~adat
            INTO w_mod
            FROM modact
            INNER JOIN modattr
              ON modattr~name = modact~name
            WHERE modact~member = wa_modsapa-name
              AND modact~typ    = space.
          IF sy-subrc = 0.
            i_userexit-modattr  = w_mod.
          ENDIF.
        ENDIF.

* BADI data
        IF  i_userexit-type EQ 'BADI'.
          CLEAR wa_sxs_attr.
          SELECT SINGLE exit_name INTO wa_sxs_attr-exit_name FROM sxs_attr
                                        WHERE exit_name = i_userexit-txt.
          IF sy-subrc = 0.
            i_userexit-modname = i_userexit-txt.
          ELSE.
            i_userexit-modname = 'Dynamic call'.            "#EC NOTEXT
          ENDIF.
          CLEAR wa_sxs_attrt.
          SELECT SINGLE text INTO wa_sxs_attrt-text FROM sxs_attrt
                                         WHERE exit_name = wa_sxs_attr-exit_name
                                           AND sprsl = sy-langu.
          i_userexit-modtext = wa_sxs_attrt-text.
        ENDIF.

* BADI Implementation
        IF  i_userexit-type EQ 'BADI'.
          SELECT COUNT( * ) FROM sxc_exit WHERE exit_name = @i_userexit-txt INTO @DATA(sxc_exit).
          DATA(w_cnt) = sy-dbcnt.
* determine id BADI is for interal or external use
          SELECT SINGLE * FROM sxs_attr WHERE exit_name = @i_userexit-txt INTO @DATA(sxs_attr).
          IF sxs_attr-internal = 'X'.
            wa_sxs_attrt-text = 'SAP '.
          ELSE.
            wa_sxs_attrt-text = 'CUST'.
          ENDIF.
*        concatenate wa_sxs_attrt-text w_cnt into i_userexit-modattr-name
*        separated by space.
          WRITE wa_sxs_attrt-text TO i_userexit-modattr-name.
          WRITE w_cnt             TO i_userexit-modattr-name+5.
        ENDIF.

        MODIFY it_userexit FROM i_userexit.
      ENDLOOP.

      IF objtype EQ 'U'.
* get enhancements via program package
        CLEAR wa_tadir.
        SELECT SINGLE devclass INTO wa_tadir-devclass FROM tadir
                                   WHERE pgmid    = 'R3TR'
                                     AND object   = 'PROG'
                                     AND obj_name = p_pname.
        IF sy-subrc = 0.
          CLEAR: wa_modsapa, wa_modsapt.
          SELECT name FROM modsapa INTO wa_modsapa-name
                                WHERE devclass = wa_tadir-devclass.
            SELECT SINGLE modtext FROM modsapt INTO wa_modsapt-modtext
                                WHERE name = wa_modsapa-name
                                  AND sprsl = sy-langu.

            CLEAR i_userexit.
            READ TABLE it_userexit WITH KEY modname = wa_modsapa-name INTO i_userexit.
            IF sy-subrc <> 0.
              i_userexit-modtext = wa_modsapt-modtext.
              i_userexit-type = 'Enhancement'.              "#EC NOTEXT
              i_userexit-modname  = wa_modsapa-name.
              i_userexit-txt = 'Determined from program DevClass'. "#EC NOTEXT
              i_userexit-pname = 'Unknown'.                 "#EC NOTEXT
              APPEND i_userexit TO it_userexit.
            ENDIF.
          ENDSELECT.
        ENDIF.
      ENDIF.

      DATA lv_funcname TYPE tftit-funcname.
      DATA lv_pname TYPE trdirt-name.

* set output.
      LOOP AT it_userexit INTO i_userexit.
        IF objtype = 'U' OR objtype = 'E'.
          wa_result-obj_name = i_userexit-modname.
          lv_funcname        = i_userexit-txt.

          SELECT SINGLE stext FROM tftit INTO @DATA(stext) WHERE spras EQ 'E' AND funcname EQ @lv_funcname.
          IF sy-subrc EQ 0 AND objtype = 'U'.
            CONCATENATE  stext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
                         i_userexit-txt INTO wa_result-ddtext SEPARATED BY space.
            CLEAR stext.
          ELSE.
            CONCATENATE i_userexit-modtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
                        i_userexit-txt INTO wa_result-ddtext SEPARATED BY space.
          ENDIF.
        ELSEIF objtype = 'B' OR objtype = 'C'.
          wa_result-obj_name = i_userexit-modname.
          wa_result-ddtext   = i_userexit-modtext.

        ELSEIF objtype = 'P' OR objtype = 'W'.
          wa_result-obj_name = i_userexit-pname.
          wa_result-ddtext   = i_userexit-txt.

        ELSEIF objtype = 'A'.
          wa_result-obj_name = i_userexit-txt.

          CONCATENATE i_userexit-modtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
                      i_userexit-pname INTO wa_result-ddtext SEPARATED BY space.
        ENDIF.

        APPEND wa_result TO it_result.
        CLEAR wa_result.
      ENDLOOP.

      CASE objtype.
        WHEN 'F'.
          LOOP AT it_fmodule INTO i_fmodule WHERE bapi NE 'X'.
            lv_funcname        = i_fmodule-name.
            SELECT SINGLE stext FROM tftit INTO @stext WHERE spras EQ 'E' AND funcname EQ @lv_funcname.
            wa_result-obj_name = i_fmodule-name.
            wa_result-ddtext   = stext.
            APPEND wa_result TO it_result.
            CLEAR: stext, wa_result.
          ENDLOOP.
          DELETE it_result WHERE ddtext IS INITIAL.
        WHEN 'I'.
          LOOP AT it_fmodule INTO i_fmodule WHERE bapi EQ 'X'.
            lv_funcname        = i_fmodule-name.
            SELECT SINGLE stext FROM tftit INTO @stext WHERE spras EQ 'E' AND funcname EQ @lv_funcname.
            wa_result-obj_name = i_fmodule-name.
            wa_result-ddtext   = stext.
            APPEND wa_result TO it_result.
            CLEAR: stext, wa_result.
          ENDLOOP.
          DELETE it_result WHERE ddtext IS INITIAL.
        WHEN 'D'.
          LOOP AT it_devclass INTO i_devclass.
            CLEAR wa_modsapa.
            SELECT name FROM modsapa INTO wa_modsapa
                         WHERE devclass = i_devclass-clas.
              SELECT SINGLE name modtext INTO CORRESPONDING FIELDS OF wa_modsapt
                                         FROM modsapt
                                           WHERE name  = wa_modsapa-name
                                           AND sprsl = sy-langu.
              wa_result-obj_name = wa_modsapa-name.
              wa_result-ddtext   = wa_modsapt-modtext.
              APPEND wa_result TO it_result.
              CLEAR: wa_result.
            ENDSELECT.
          ENDLOOP.
        WHEN 'S'.
          LOOP AT it_submit INTO i_submit.
            wa_result-obj_name =  i_submit-pname.
            SELECT SINGLE text FROM trdirt INTO @wa_result-ddtext WHERE sprsl EQ 'E' AND name EQ @i_submit-pname.
            APPEND wa_result TO it_result.
            CLEAR: wa_result.
          ENDLOOP.
          DELETE it_result WHERE ddtext IS INITIAL.
      ENDCASE.

      /ui2/cl_json=>serialize(
        EXPORTING
          data   = it_result             " Data to serialize
        RECEIVING
          r_json = result           " JSON string
      ).

    ENDIF.

  ENDMETHOD.


  METHOD get_objct_tabl.

    DATA:dref TYPE REF TO data.
    FIELD-SYMBOLS:<table> TYPE STANDARD TABLE.
    TYPE-POOLS: slis, icon.

    FIELD-SYMBOLS:<fs_tab13> TYPE mandt.
    DATA:wa_dd1      TYPE dd03l,
         wa_dd2      TYPE dd03l,
         wa_dd3      TYPE dd03l,
         wa_dd4      TYPE dd03l,
         lv_tab      TYPE dd03l-tabname,
         wa_dd_final TYPE dd03l,
         s_exc       TYPE boole-boole VALUE '/',
         s_field1    TYPE dd03l-fieldname,
         s_field2    TYPE dd03l-fieldname,
         s_field3    TYPE dd03l-fieldname,
         s_field4    TYPE dd03l-fieldname.

    s_field1  =  field1.
    s_field2  =  field2.
    s_field3  =  field3.
    s_field4  =  field4.

    IF s_exc IS NOT INITIAL.
      lv_tab = '/%'.
    ENDIF.

    IF s_field2 IS INITIAL.
      s_field2 = s_field1.
    ENDIF.

* Internal Tables
    TYPES: BEGIN OF t_ialv,
             tabname    TYPE dd03l-tabname,
             ddtext     TYPE dd02t-ddtext,
             ddlname    TYPE ddldependency-ddlname,
             objectname TYPE ddldependency-objectname,
             tbtext     TYPE ddddlsrct-ddtext,
             field1     TYPE dd03l-fieldname,
             field2     TYPE dd03l-fieldname,
             field3     TYPE dd03l-fieldname,
             field4     TYPE dd03l-fieldname,
             count(1)   TYPE c,
           END OF t_ialv .

    DATA: ialv   TYPE STANDARD TABLE OF t_ialv,
          wa_alv TYPE t_ialv.

    IF s_field1 IS NOT INITIAL.
      SELECT c~tabname
        FROM ( ( dd03l AS c
             INNER JOIN dd02v AS p ON p~tabname  = c~tabname
                                  AND p~tabclass = 'TRANSP')
*                              AND p~cityto   = @cityto )
             INNER JOIN dd09l AS f ON f~tabname = p~tabname )
        WHERE ( ( c~rollname EQ @s_field1 )
        OR ( c~fieldname EQ @s_field1  ) )
        AND p~tabname NOT LIKE @lv_tab
           INTO TABLE @DATA(itab13).
    ENDIF.
    IF s_field2 IS NOT INITIAL.
      SELECT c~tabname
      FROM ( ( dd03l AS c
           INNER JOIN dd02v AS p ON p~tabname  = c~tabname
                                AND p~tabclass = 'TRANSP')
*                              AND p~cityto   = @cityto )
           INNER JOIN dd09l AS f ON f~tabname = p~tabname )
      WHERE ( ( c~rollname EQ @s_field2 )
      OR ( c~fieldname EQ @s_field2  )    )
      AND p~tabname NOT LIKE @lv_tab
         INTO TABLE @DATA(itab14).
    ENDIF.
    IF s_field3 IS NOT INITIAL.
      SELECT c~tabname
      FROM ( ( dd03l AS c
           INNER JOIN dd02v AS p ON p~tabname  = c~tabname
                                AND p~tabclass = 'TRANSP')
           INNER JOIN dd09l AS f ON f~tabname = p~tabname )
      WHERE ( ( c~rollname EQ @s_field3 )

      OR ( c~fieldname EQ @s_field3  )    )
      AND p~tabname NOT LIKE @lv_tab
         INTO TABLE @DATA(itab15).
    ENDIF.
    IF s_field4 IS NOT INITIAL.
      SELECT c~tabname
    FROM ( ( dd03l AS c
         INNER JOIN dd02v AS p ON p~tabname  = c~tabname
                              AND p~tabclass = 'TRANSP')
*                              AND p~cityto   = @cityto )
         INNER JOIN dd09l AS f ON f~tabname = p~tabname )
    WHERE ( ( c~rollname EQ @s_field4 )
    OR ( c~fieldname EQ @s_field4  )
    )
    AND p~tabname NOT LIKE @lv_tab
*                                AND f~connid = p~connid )
*       ORDER BY c~carrname, p~connid, f~fldate
       INTO TABLE @DATA(itab16).
    ENDIF.
    SORT itab13 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab13 COMPARING tabname..
    SORT itab14 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab14 COMPARING tabname..
    SORT itab15 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab15 COMPARING tabname..
    SORT itab16 BY tabname.
    DELETE ADJACENT DUPLICATES FROM itab16 COMPARING tabname.
    IF s_field1 IS NOT INITIAL.

      LOOP AT itab13 INTO DATA(wa_tab133).
        IF itab14[] IS NOT INITIAL.
          READ TABLE itab14  INTO DATA(wa_tab134) WITH KEY tabname = wa_tab133-tabname.
          IF sy-subrc = 0.
            IF itab15[] IS NOT INITIAL.
              READ TABLE itab15  INTO DATA(wa_tab135) WITH KEY tabname = wa_tab133-tabname.
              IF sy-subrc = 0.
                IF itab16[] IS NOT INITIAL.
                  READ TABLE itab16  INTO DATA(wa_tab136) WITH KEY tabname = wa_tab133-tabname.
                  IF sy-subrc = 0.
                    wa_alv-tabname =  wa_tab134-tabname.
                    wa_alv-field1 =  |?{ s_field1 }|.
                    wa_alv-field2 =  |?{ s_field2 }|.
                    wa_alv-field3 =  |?{ s_field3 }|.
                    wa_alv-field4 =  |?{ s_field4 }|.
                    wa_alv-count  =  '4'.
                    APPEND wa_alv TO ialv.
                    CLEAR wa_alv.
                  ELSE.

                    wa_alv-tabname =  wa_tab134-tabname.
                    wa_alv-field1 =  |?{ s_field1 }|.
                    wa_alv-field2 =  |?{ s_field2 }|.
                    wa_alv-field3 =  |?{ s_field3 }|.
                    wa_alv-count  =  '3'.
                    APPEND wa_alv TO ialv.
                    CLEAR wa_alv.
                  ENDIF.
                ELSE.

                  wa_alv-tabname =  wa_tab134-tabname.
                  wa_alv-field1 =  |?{ s_field1 }|.
                  wa_alv-field2 =  |?{ s_field2 }|.
                  wa_alv-field3 =  |?{ s_field3 }|.
                  wa_alv-count  =  '3'.

                  APPEND wa_alv TO ialv.
                  CLEAR wa_alv.
                ENDIF.
              ELSE.

                wa_alv-tabname =  wa_tab134-tabname.
                wa_alv-field1 =  |?{ s_field1 }|.
                wa_alv-field2 =  |?{ s_field2 }|.
                wa_alv-count  =  '2'.

                APPEND wa_alv TO ialv.
                CLEAR wa_alv.
              ENDIF.

            ELSE.
              wa_alv-tabname =  wa_tab134-tabname.
              wa_alv-field1 =  |?{ s_field1 }|.
              wa_alv-field2 =  |?{ s_field2 }|.
              wa_alv-count  =  '2'.
              APPEND wa_alv TO ialv.
              CLEAR wa_tab134.
              CLEAR wa_alv.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.
    DELETE ADJACENT DUPLICATES FROM ialv COMPARING ALL FIELDS.
    SORT ialv DESCENDING BY count.

    LOOP AT ialv INTO wa_alv.

      SELECT SINGLE ddtext FROM dd02t INTO wa_alv-tbtext WHERE tabname EQ wa_alv-tabname AND ddlanguage EQ 'E'.

      wa_result-obj_name = wa_alv-tabname.

      IF wa_alv-field1 EQ wa_alv-field2.
        CONCATENATE wa_alv-tbtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
        wa_alv-field1 wa_alv-field3 wa_alv-field4 INTO wa_result-ddtext SEPARATED BY space.
      ELSE.
        CONCATENATE wa_alv-tbtext cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
        wa_alv-field1 wa_alv-field2 wa_alv-field3 wa_alv-field4  INTO wa_result-ddtext SEPARATED BY space.
      ENDIF.
      MODIFY ialv FROM wa_alv.
      APPEND wa_result TO it_result.
      CLEAR: wa_alv, wa_result.
    ENDLOOP.

    /ui2/cl_json=>serialize(
      EXPORTING
        data   = it_result             " Data to serialize
      RECEIVING
        r_json = result           " JSON string
    ).

  ENDMETHOD.


  METHOD if_rest_resource~get.
*CALL METHOD SUPER->IF_REST_RESOURCE~GET
*    .
    DATA: p_tcode     TYPE sobj_name,
          p_pname     TYPE sobj_name,
          p_object(3) TYPE c,
          lv_result   TYPE string,
          ddic(4)     TYPE c,
          field1      TYPE dd03l-fieldname,
          field2      TYPE dd03l-fieldname,
          field3      TYPE dd03l-fieldname,
          field4      TYPE dd03l-fieldname.

    p_object        = mo_request->get_uri_query_parameter( iv_name = 'OBJECT' ).
    ddic            = mo_request->get_uri_query_parameter( iv_name = 'DDIC' ).
    p_tcode         = to_upper( mo_request->get_uri_query_parameter( iv_name = 'TCODE' ) ).
    p_pname         = to_upper( mo_request->get_uri_query_parameter( iv_name = 'PGMNA' ) ).
    field1          = to_upper( mo_request->get_uri_query_parameter( iv_name = 'F1' ) ).
    field2          = to_upper( mo_request->get_uri_query_parameter( iv_name = 'F2' ) ).
    field3          = to_upper( mo_request->get_uri_query_parameter( iv_name = 'F3' ) ).
    field4          = to_upper( mo_request->get_uri_query_parameter( iv_name = 'F4' ) ).

    IF p_object IS NOT INITIAL.

      CALL METHOD get_objct_data
        EXPORTING
          objtype = p_object
          tcode   = p_tcode
          pname   = p_pname
        IMPORTING
          result  = lv_result.
    ELSEIF p_object IS INITIAL AND ddic EQ 'CDS'.

      CALL METHOD get_objct_cds
        EXPORTING
          field1 = field1
          field2 = field2
          field3 = field3
          field4 = field4
        IMPORTING
          result = lv_result.

    ELSEIF p_object IS INITIAL AND ddic EQ 'TABL'.

      CALL METHOD get_objct_tabl
        EXPORTING
          field1 = field1
          field2 = field2
          field3 = field3
          field4 = field4
        IMPORTING
          result = lv_result.
    ENDIF.

    mo_response->create_entity( )->set_string_data( iv_data = lv_result ).
    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'Content-Type'  " Header Name
        iv_value = 'application/json' " Header Value
    ).

    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'Access-Control-Allow-Origin'  " Header Name
        iv_value =  mo_request->get_header_field( iv_name = 'Origin' ) " Header Value
    ).

    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'Access-Control-Allow-Credentials'  " Header Name
        iv_value = 'true' " Header Value
    ).

    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'Access-Control-Allow-Methods'  " Header Name
        iv_value = 'GET' " Header Value
    ).

    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'Access-Control-Allow-Headers'  " Header Name
        iv_value = 'Content-Type, Authorization' " Header Value
    ).

  ENDMETHOD.
ENDCLASS.
