*&---------------------------------------------------------------------*
*& Report  ZDTA50A0
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50A0.
*DATA: var1 type c length 20,
*var2 type c length 20,
*var3 type c length 50.
*var2 = sy-datum+6(2).
*var1 = sy-datum+4(2).
*CONCATENATE 'Día:' sy-datum+6(2) 'Mes:' sy-datum+4(2) 'Año:' sy-datum(4) INTO var3 SEPARATED by space.
*CONCATENATE 'Fecha:' sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO var2.
*CONCATENATE 'Hora:' sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) into var1.
*write var3. skip.
*write var2. skip.
*write var1.
TYPES: BEGIN OF var5,
         date TYPE c LENGTH 40,
         hour   TYPE c LENGTH 50,
       END OF var5.
data var55 type var5.
CONCATENATE 'Día:' sy-datum+6(2) 'Mes:' sy-datum+4(2) 'Año:' sy-datum(4) INTO var55-date SEPARATED by space.
CONCATENATE 'Hora:' sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) into var55-hour.
write var55.

select CARRID, CONNID, AIRPFROM, DEPTIME, AIRPTO, ARRTIME

*&---------------------------------------------------------------------*
*& Report  ZDTA50B1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZDTA50B1.
TABLES SPFLI.
SELECT * FROM SPFLI.
  write:/ 'Compañía aérea - ', SPFLI-CARRID, 'Conexión - ', SPFLI-CONNID, 'Aeropuerto de salida:Hora de salida - ', SPFLI-AIRPFROM, SPFLI-DEPTIME, 'Aeropuerto de llegada:Hora de llegada', SPFLI-AIRPTO, SPFLI-ARRTIME.
ENDSELECT.

CARRID, CONNID, FLDATE
*&---------------------------------------------------------------------*
*& Report  ZDTA50B2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZDTA50B2.
TABLES SBOOK.
SELECT * FROM SBOOK WHERE CARRID = 'LH' AND CONNID = 0400 AND SMOKER NE NULL.
IF SY-SUBRC NE 0.
WRITE 'No hay datos'.
ELSE.
WRITE:/ SBOOK-CONNID.
ENDIF.
ENDSELECT.

SPFLI
CARRID, CONNID, COUNTRYFR, CITYFROM, AIRPFROM, COUNTRYTO, CITYTO, 
AIRPTO, FLTIME, DEPTIME, ARRTIME, DISTANCE, DISTID, FLTYPE, PERIOD
SBOOK
CARRID, CONNID, FLDATE, BOOKID, CUSTOMID, CUSTTYPE, SMOKER, LUGGWEIGHT, 
WUNIT, INVOICE, CLASS, FORCURAM, FORCURKEY, LOCCURAM, LOCCURKEY, 
ORDER_DATE, COUNTER, AGENCYNUM, CANCELLED, RESERVED, PASSNAME, PASSFORM, PASSBIRTH

SBOOK-CARRID, SBOOK-CONNID, SBOOK-FLDATE, SBOOK-FLDATE, SBOOK-BOOKID, SPFLI-CITYFROM, SPFLI-CITYTO
SELECT * FROM SPFLI INNER JOIN SBOOK ON SPFLI-CARRID = SBOOK-CARRID AND SPFLI-CONNID = SBOOK-CONNID.

*&---------------------------------------------------------------------*
*& Report  ZDTA50B4
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdta50b4.
DATA: ls_spfli TYPE spfli,
      ls_sbook TYPE sbook.
SELECT: a~carrid a~connid b~fldate b~bookid a~cityfrom a~cityto 
 INTO
   (ls_spfli-carrid, 
    ls_spfli-connid, 
    ls_sbook-fldate, 
    ls_sbook-bookid, 
    ls_spfli-cityfrom, 
    ls_spfli-cityto)
  FROM spfli AS a INNER JOIN sbook AS b
  ON a~carrid = b~carrid AND a~connid = b~connid
  WHERE a~cityfrom = 'FRANKFURT'.
  WRITE:/ 
    ls_spfli-carrid, 
    ' - ', 
    ls_spfli-connid, 
    ' - ', 
    ls_sbook-fldate, 
    ' - ', 
    ls_sbook-bookid, 
    ' - ', 
    ls_spfli-cityfrom, 
    ' - ', 
    ls_spfli-cityto.
ENDSELECT.



*&---------------------------------------------------------------------*
*& Report  ZDTA50H1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50H1.
TYPES: BEGIN OF ty_data,
    CARRID TYPE SPFLI-CARRID,
    CONNID   TYPE SPFLI-CONNID,
    CITYFROM TYPE SPFLI-CITYFROM,
    CITYTO TYPE SPFLI-CITYTO,
END OF ty_data.

DATA table_name TYPE ty_data.
SELECT CARRID CONNID CITYFROM CITYTO INTO table_name
FROM SPFLI
ORDER BY CARRID CITYFROM CITYTO.
WRITE:/ 
table_name-CARRID, ' - ', 
table_name-CONNID,  ' - ', 
table_name-CITYFROM, ' - ', 
table_name-CITYTO, ' - '.
ENDSELECT.

SFLIGHT
SEATSOCC.

*&---------------------------------------------------------------------*
*& Report  ZDTA50H2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50H2.
TYPES: BEGIN OF ty_data,
  MAXIM TYPE I,
  MINIM TYPE I,
  AVEG TYPE I,
END OF ty_data.
DATA table_name TYPE ty_data.
SELECT SINGLE MAX( SEATSOCC ) 
  MIN( SEATSOCC ) 
  AVG( SEATSOCC ) INTO table_name
FROM SFLIGHT.
WRITE: 'Máximo: ', table_name-MAXIM, ' - Mínimo: ', table_name-MINIM, ' - Media: ', table_name-AVEG.


*&---------------------------------------------------------------------*
*& Report  ZDTA50H3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50H3.
DATA: BEGIN OF table_name,
        connid    TYPE SPFLI-CONNID,
        cityfrom  TYPE SPFLI-CITYFROM,
        cityto    TYPE SPFLI-CITYTO,
        countryfr TYPE SPFLI-COUNTRYFR,
        airpfrom  TYPE SPFLI-AIRPFROM,
        countryto TYPE SPFLI-COUNTRYTO,
        airpto    TYPE SPFLI-AIRPTO,
        fltime    TYPE SPFLI-FLTIME,
        deptime   TYPE SPFLI-DEPTIME,
        arrtime   TYPE SPFLI-ARRTIME,
        distance  TYPE SPFLI-DISTANCE,
        distid    TYPE SPFLI-DISTID,
        fltype    TYPE SPFLI-FLTYPE,
        period    TYPE SPFLI-PERIOD,
      END OF table_name.

EXEC SQL PERFORMING loop_output.
  SELECT CONNID, CITYFROM, CITYTO, COUNTRYFR, AIRPFROM, COUNTRYTO, 
      AIRPTO, FLTIME, DEPTIME, ARRTIME, DISTANCE, DISTID, FLTYPE, PERIOD
  INTO   :table_name
  FROM   SPFLI
  WHERE  CARRID = 'LH' AND CONNID = 0400
ENDEXEC.

FORM loop_output.
  WRITE: / table_name-connid, table_name-cityfrom, table_name-cityto, 
  table_name-countryfr, table_name-airpfrom, table_name-countryto, 
  table_name-airpto, table_name-fltime, table_name-deptime, table_name-arrtime, 
  table_name-distance,  table_name-distid, table_name-fltype, table_name-period.
ENDFORM.

*&---------------------------------------------------------------------*
*& Report  ZDTA50F1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50F1.
TABLES: SFLIGHT.
DATA: TI_SFLIGHT TYPE TABLE OF ZDA50,
      WA_SFLIGHT LIKE LINE OF TI_SFLIGHT.
SELECT CARRID SUM( SEATSMAX ) AS SEATSMAX
  INTO CORRESPONDING FIELDS OF TABLE TI_SFLIGHT
  UP TO 300 ROWS
  FROM SFLIGHT
  GROUP BY CARRID.

LOOP AT TI_SFLIGHT INTO WA_SFLIGHT.
WRITE:/ WA_SFLIGHT-CARRID, WA_SFLIGHT-SEATSMAX.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Report  ZDTA50F2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50F2.
TABLES: SFLIGHT.
DATA: TABLA2 TYPE TABLE OF SFLIGHT WITH HEADER LINE.

SELECT * INTO TABLE TABLA2 FROM SFLIGHT.

LOOP AT TABLA2.
WRITE:/ TABLA2-CARRID, TABLA2-CONNID, TABLA2-FLDATE, TABLA2-PRICE.
ENDLOOP.
SKIP.
DELETE TABLA2 where CARRID ne 'LH'.

SELECT * INTO TABLE TABLA2 FROM SFLIGHT WHERE CARRID = 'LH' AND CONNID = 0401.

WRITE 'Solo LH'. skip.
READ TABLE TABLA2 INDEX 1.
TABLA2-PRICE = TABLA2-PRICE * 3.
MODIFY TABLA2 INDEX 1.
WRITE:/ TABLA2-CARRID, TABLA2-CONNID, TABLA2-FLDATE, TABLA2-PRICE.
SKIP.
WRITE 'despues de multiplicar'.

LOOP AT TABLA2 WHERE CONNID = 0401.
WRITE:/ TABLA2-CARRID, TABLA2-CONNID, TABLA2-FLDATE, TABLA2-PRICE.
ENDLOOP.

FUNCTION Z_LETRA_NIF_50.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(NIF) TYPE  I
*"  EXPORTING
*"     REFERENCE(LETRA) TYPE  C
*"     REFERENCE(EX_RESULTADO) TYPE  BAPIRETURN
*"----------------------------------------------------------------------
  DATA: valor_resto TYPE i.


  clear EX_RESULTADO.
  valor_resto = nif MOD 23.

  SELECT letra
    FROM zletranif
    INTO letra
   WHERE resto = valor_resto.
  endselect.

  IF sy-subrc = 0.
    letra = letra.
  else.
    ex_resultado-code = 1.
    ex_resultado-message = text-001.
  ENDIF.
ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Report  ZDTA50J0
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50J0.
DATA LETRA_DNI TYPE C.
PARAMETERS NIF_L TYPE I.
CALL FUNCTION 'Z_LETRA_NIF_50'
EXPORTING NIF = NIF_L
IMPORTING LETRA = LETRA_DNI.
WRITE:/ 'NIF        ', 'LETRA'.
WRITE :/ NIF_L, LETRA_DNI.


*&---------------------------------------------------------------------*
*& Report  ZDTA50J1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50J1.
DATA TABLA1 TYPE TABLE OF SFLIGHT WITH HEADER LINE.

SELECT * INTO TABLA1 FROM SFLIGHT.
WRITE:/ TABLA1-PRICE.
ENDSELECT.

Sin subrutina
*&---------------------------------------------------------------------*
*& Report  ZDTA50J1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50J1.
PARAMETERS company TYPE SFLIGHT-CARRID.
DATA: BEGIN OF SUMA,
  SUMA1 TYPE I,
  MANDT TYPE C,
END OF SUMA.

DATA TABLA1 TYPE TABLE OF SFLIGHT WITH HEADER LINE.

SELECT CARRID CONNID SUM( PAYMENTSUM ) AS PAYMENTSUM INTO CORRESPONDING FIELDS OF TABLA1
  FROM SFLIGHT WHERE CARRID = company GROUP BY CONNID CARRID ORDER BY CARRID ASCENDING.
WRITE:/ TABLA1-CARRID, TABLA1-CONNID, TABLA1-PAYMENTSUM.
ENDSELECT.

*&---------------------------------------------------------------------*
*& Report  ZDTA50J2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50J2.
TABLES SFLIGHT.
PARAMETERS currency TYPE SFLIGHT-CURRENCY.
SELECT-OPTIONS company FOR SFLIGHT-CARRID.
*Definimos los tipos.
TYPES: BEGIN OF TIPO,
  CARRID TYPE SFLIGHT-CARRID,
  CURRENCY TYPE SFLIGHT-CURRENCY,
  PAYMENTSUM TYPE SFLIGHT-PAYMENTSUM,
END OF TIPO.
*Creamos los registros.
DATA: TRATADOS TYPE STANDARD TABLE OF TIPO,
      TOTAL TYPE SFLIGHT-PAYMENTSUM,
      CANTIDAD_LOCAL TYPE SFLIGHT-PAYMENTSUM,
      TEMPORAL TYPE TIPO OCCURS 0,
      GRABADO TYPE TIPO.
      TOTAL = 0.
PERFORM SUBROUT TABLES TEMPORAL.
WRITE: / 'Transp.', '       VolNeg', '        Moneda'.
LOOP AT TEMPORAL INTO GRABADO.
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
  EXPORTING
    DATE = SY-DATUM
    FOREIGN_AMOUNT = GRABADO-PAYMENTSUM
    FOREIGN_CURRENCY = GRABADO-CURRENCY
    LOCAL_CURRENCY = currency
  IMPORTING
    LOCAL_AMOUNT = CANTIDAD_LOCAL
*Manejo de errores.
  EXCEPTIONS
    NO_RATE_FOUND = 1
    OVERFLOW = 2
    NO_FACTORS_FOUND = 3
    NO_SPREAD_FOUND = 4
    DERIVED_2_TIMES = 5
    OTHERS = 6.
  IF SY-SUBRC <> 0.
    WRITE: / 'ERROR'.
  ENDIF.
    TOTAL = TOTAL + CANTIDAD_LOCAL.
  WRITE: / GRABADO-CARRID, '  ', GRABADO-PAYMENTSUM, GRABADO-CURRENCY.
ENDLOOP.
SKIP.
WRITE: / 'TOTAL:', TOTAL, currency.
*Definicion de subrutina.
FORM SUBROUT TABLES DATOS_INVENTADOS.
SELECT * INTO CORRESPONDING FIELDS OF DATOS_INVENTADOS
  FROM SFLIGHT
  WHERE CARRID IN COMPANY.
  COLLECT DATOS_INVENTADOS.
  ENDSELECT.
ENDFORM.

FUNCTION z_porcen50.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(LL_SEATSMAX) TYPE  SFLIGHT-SEATSMAX
*"     REFERENCE(LL_SEATSOCC) TYPE  SFLIGHT-SEATSOCC
*"  EXPORTING
*"     REFERENCE(L_PORCENTAJE) TYPE  P
*"     REFERENCE(LR_RESULTADO) TYPE  BAPIRETURN
*"----------------------------------------------------------------------

  CLEAR lr_resultado.

  l_porcentaje = 100.
  IF ll_seatsmax <> 0.
    l_porcentaje = ( ll_seatsocc / ll_seatsmax ) * 100.
  else.
    lr_resultado-code    = 1.
    lr_resultado-message = text-002.
  ENDIF.

ENDFUNCTION.
SFLIGHT-SEATSMAX -> Maximo de plazas por vuelo.
SFLIGHT-SEATSOCC -> Plazas ocupadas.

Metodo 2
*&---------------------------------------------------------------------*
*& Report  ZDTA03J3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA03J3.
*Report que muestra los porcentajes de ocupación  de los vuelos.
*Tenems que escribir un módulo de función que calcule el porcentaje.
TABLES: SFLIGHT.
WRITE: / 'COMPAÑIA', 'CONEXIÓN', 'FECHA DE VUELO', 'OCUPACIÓN MÁXIMA', 'PLAZAS OCUPADAS',
'PORCENTAJE DE ACUPACIÓN'.
TYPES: BEGIN OF TI,
  CARRID TYPE SFLIGHT-CARRID,
  CONNID TYPE SFLIGHT-CONNID,
  FLDATE TYPE SFLIGHT-FLDATE,
  SEATSMAX TYPE SFLIGHT-SEATSMAX,
  SEATSOCC TYPE SFLIGHT-SEATSOCC,
  PORCEN TYPE S_PRICE,
  END OF TI.
DATA: GT_TI TYPE STANDARD TABLE OF TI,
       G_TI TYPE TI.

SELECT-OPTIONS: COMPANY FOR SFLIGHT-CARRID.
SELECT CARRID CONNID FLDATE SEATSMAX SEATSOCC FROM SFLIGHT
  INTO TABLE GT_TI
  WHERE CARRID IN COMPANY.
  LOOP AT GT_TI INTO G_TI.
    CALL FUNCTION 'Z_PORCEN03'
    EXPORTING
      IMP_SEATSMAX = G_TI-SEATSMAX
      IMP_SEATSOCC = G_TI-SEATSOCC
    IMPORTING
      EX_PORCEN = G_TI-PORCEN.
    WRITE: / G_TI-CARRID, G_TI-CONNID, G_TI-FLDATE, G_TI-SEATSMAX, G_TI-SEATSOCC,
    G_TI-PORCEN, '%'.
    ENDLOOP.
	
*&---------------------------------------------------------------------*
*& Report  ZDTA50J3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50J3.
DATA: int_table TYPE STANDARD TABLE OF SFLIGHT WITH HEADER LINE,
wa_table LIKE LINE OF int_table,
SEATSMAX TYPE SFLIGHT-SEATSMAX,
SEATSOCC TYPE SFLIGHT-SEATSOCC,
PORCEN TYPE P.
TABLES SFLIGHT.
SELECT-OPTIONS: COMPANY FOR SFLIGHT-CARRID.

WRITE:/ '   ', 'Con.', 'FE.Vuelo', '      Ocupación máx.  ', 'Plazas ocupadas   ', 'Capacidad'.
SELECT *
INTO int_table
FROM SFLIGHT
WHERE CARRID IN company.
CALL FUNCTION 'Z_PORCEN50'
EXPORTING
  LL_SEATSMAX = int_table-SEATSMAX
  LL_SEATSOCC = int_table-SEATSOCC
IMPORTING
  L_PORCENTAJE = PORCEN.
WRITE:/ int_table-CARRID, 
int_table-CONNID, 
int_table-FLDATE,
int_table-SEATSMAX,
'       ',
int_table-SEATSOCC,
PORCEN, '%'.
ENDSELECT.

*&---------------------------------------------------------------------*
*& Report  ZDTA50D1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50D1.
PARAMETERS: fecha TYPE SY-DATUM,
dias TYPE I.
DATA: DIA TYPE I,
      MES TYPE C.

DIA = fecha+6(2).
DIA = DIA + dias.
MES = fecha+4(2).
IF MES = 01 OR  MES = 03 OR MES = 05 OR MES = 07 OR MES = 08 OR MES = 10 OR MES = 12.
 
WRITE:/ fecha(4), '/', fecha+4(2), '/', DIA, '/'.

*&---------------------------------------------------------------------*
*& Report  ZDTA50D1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50D1.
PARAMETERS: fecha TYPE SY-DATUM,
dias TYPE T5A4A-DLYDY.
DATA: resultado TYPE SY-DATUM.
CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
EXPORTING
date = fecha "La fecha base
signum = '+' "El signo suma o resta a la fecha base
days = dias "Los dias a sumar o restar si no se van a modificar se deja cero
months = 0 "Los meses a sumar o restar
years = 0 "Los años a sumar o restar
IMPORTING
calc_date = resultado. "El nombre de tu variable typo fecha
WRITE:/ 'Día: ', resultado+6(2).
WRITE:/ 'Mes: ', resultado+4(2).
WRITE:/ 'Año: ', resultado(4).


*&---------------------------------------------------------------------*
*& Report  ZDTA50D2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50D2.
PARAMETERS: numero_a TYPE I, operador TYPE C, numero_b TYPE I.
DATA: resultado TYPE P DECIMALS 2,
      resultado2 TYPE STRING,
      respuesta TYPE STRING.

CASE operador.
WHEN '/'.
resultado = numero_a / numero_b.
resultado2 = resultado.
CONCATENATE 'Resultado: ' resultado2 INTO respuesta SEPARATED BY SPACE.
WHEN '*'.
resultado = numero_a * numero_b.
resultado2 = resultado.
CONCATENATE 'Resultado: ' resultado2 INTO respuesta SEPARATED BY SPACE.
WHEN '+'.
resultado = numero_a + numero_b.
resultado2 = resultado.
CONCATENATE 'Resultado: ' resultado2 INTO respuesta SEPARATED BY SPACE.
WHEN '-'.
resultado = numero_a - numero_b.

CONCATENATE 'Resultado: ' resultado2 INTO respuesta SEPARATED BY SPACE.
WHEN OTHERS.
respuesta = 'Operador no válido'.
ENDCASE.
CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
EXPORTING
textline1 = respuesta.

*&---------------------------------------------------------------------*
*& Report  ZDTA50D2B
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50D2B.
SELECTION-SCREEN BEGIN OF BLOCK BLOQUE1 WITH FRAME TITLE text-001.
PARAMETERS : Report RADIOBUTTON GROUP RB1,
Funcion RADIOBUTTON GROUP RB1.
SELECTION-SCREEN END OF BLOCK BLOQUE1.
SELECTION-SCREEN BEGIN OF BLOCK BLOQUE2 WITH FRAME TITLE text-002.
PARAMETERS: numero_a TYPE I, operador TYPE C, numero_b TYPE I.
DATA: resultado TYPE P DECIMALS 2,
      resultado2 TYPE STRING,
      respuesta TYPE ZOE_TS_DE_NUMCON2DECI,
      respuesta2 TYPE STRING.
SELECTION-SCREEN END OF BLOCK BLOQUE2.
  IF Report = 'X'.
    CASE operador.
    WHEN '/'.
    resultado = numero_a / numero_b.
    resultado2 = resultado.
    CONCATENATE 'Resultado: ' resultado2 INTO respuesta2 SEPARATED BY SPACE.
    WHEN '*'.
    resultado = numero_a * numero_b.
    resultado2 = resultado.
    CONCATENATE 'Resultado: ' resultado2 INTO respuesta2 SEPARATED BY SPACE.
    WHEN '+'.
    resultado = numero_a + numero_b.
    resultado2 = resultado.
    CONCATENATE 'Resultado: ' resultado2 INTO respuesta2 SEPARATED BY SPACE.
    WHEN '-'.
    resultado = numero_a - numero_b.

    CONCATENATE 'Resultado: ' resultado2 INTO respuesta2 SEPARATED BY SPACE.
    WHEN OTHERS.
    respuesta2 = 'Operador no válido'.
    ENDCASE.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
    EXPORTING
    textline1 = respuesta2.
  ELSEIF Funcion = 'X'.
    CALL FUNCTION 'Z_CALCULADORA00'
    EXPORTING
    OPERANDO1 = numero_a
    OPERADOR = operador
    OPERANDO2 = numero_b
    IMPORTING
    RESULTADO = respuesta.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
    EXPORTING
    textline1 = respuesta.
  ENDIF.
  
*&---------------------------------------------------------------------*
*& Report  ZDTA50G1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50G1.
TABLES SPFLI.
PARAMETERS: ciudad TYPE SPFLI-CITYFROM.
SELECT-OPTIONS: comp FOR SPFLI-CARRID.

TYPES: BEGIN OF CLASS,
CARRID TYPE SPFLI-CARRID,
CONNID TYPE SPFLI-CONNID,
CITYFROM TYPE SPFLI-CITYFROM,
CITYTO TYPE SPFLI-CITYTO,
DEPTIME TYPE SPFLI-DEPTIME,
ARRTIME TYPE SPFLI-ARRTIME,
END OF CLASS.
DATA: lt_spfli TYPE TABLE OF CLASS,
wa_spfli LIKE LINE OF lt_spfli.

SELECT CARRID CONNID CITYFROM CITYTO DEPTIME ARRTIME INTO wa_spfli
FROM SPFLI WHERE CITYFROM = ciudad AND CARRID IN comp.
WRITE:/ wa_spfli-CARRID, wa_spfli-CONNID, wa_spfli-CITYFROM, wa_spfli-CITYTO, wa_spfli-DEPTIME,
wa_spfli-ARRTIME.
ENDSELECT.

*&---------------------------------------------------------------------*
*& Report  ZDTA50K1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K1.
TABLES: sflight.

DATA: g_it_sflight TYPE TABLE OF sflight,
      g_st_sflight TYPE sflight.

DATA: g_it_screen TYPE TABLE OF screen.

PARAMETERS: company TYPE sflight-carrid MODIF ID MD1,
            con TYPE sflight-connid MODIF ID MD1.
SELECTION-SCREEN SKIP 1.

PARAMETERS: c0mpany TYPE sflight-carrid MODIF ID MD2,
            c0n TYPE sflight-connid MODIF ID MD2.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: fecha FOR sflight-fldate MODIF ID MD2,
                precio FOR sflight-price MODIF ID MD2.
PARAMETERS: moneda TYPE sflight-currency MODIF ID MD2.

AT SELECTION-SCREEN OUTPUT.
  IF company EQ ''.
    LOOP AT SCREEN.
      IF screen-group1 = 'MD2'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'MD1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  APPEND screen TO g_it_screen.
  
  
  
  *&---------------------------------------------------------------------*
*&  Include           ZDTA81K1_INC_SEL
*&--------------------

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: company FOR spfli-carrid,
                conex FOR spfli-connid.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: ordline RADIOBUTTON GROUP 1 DEFAULT 'X',
            ordconex RADIOBUTTON GROUP 1.
SELECTION-SCREEN end of block b2.

*&---------------------------------------------------------------------*
*&  Include           ZDTA81K1_INC_TOP
*&---------------------------------------------------------------------*
TABLES: spfli, sflight.
TYPES:
    BEGIN OF t_spfli,
    carrid TYPE spfli-carrid,
    connid TYPE spfli-connid,
    cityfrom TYPE spfli-cityfrom,
    cityto TYPE spfli-cityto,
    deptime TYPE spfli-deptime,
    arrtime TYPE spfli-arrtime,
    END OF t_spfli.
DATA:
      gt_t_spfli TYPE STANDARD TABLE OF t_spfli,
      g_t_spfli TYPE t_spfli.

*
TYPES:    BEGIN OF t_sflight,
    carrid TYPE sflight-carrid,
    connid TYPE sflight-connid,
    fldate TYPE sflight-fldate,
    price TYPE sflight-price,
    currency TYPE sflight-currency,
    seatsmax TYPE sflight-seatsmax,
    seatsocc TYPE sflight-seatsocc,
    seatsfree TYPE sflight-seatsmax,
    END OF t_sflight.
DATA:
      gt_t_sflight TYPE STANDARD TABLE OF t_sflight,
      g_t_sflight TYPE t_sflight.

*********************************************************************
* Definición de variables
*********************************************************************
DATA: g_carrid TYPE spfli-carrid,
      g_connid TYPE spfli-connid.
*      g_order(6) TYPE c,


*&---------------------------------------------------------------------*
*& Report  ZDTA50K1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K1.
TABLES: SPFLI.
SELECT-OPTIONS: carrids FOR SPFLI-carrid,
                connids FOR SPFLI-connid.
TYPES: BEGIN OF tipo1,
  carrid TYPE SPFLI-carrid,
  connid TYPE SPFLI-connid,
  cityfrom TYPE SPFLI-cityfrom,
  cityto TYPE SPFLI-cityto,
  deptime TYPE SPFLI-deptime,
  arrtime TYPE SPFLI-arrtime,
END OF tipo1.

  DATA: lt_select1 TYPE tipo1.
 DATA: g_carrid TYPE SPFLI-carrid,
       g_connid TYPE SPFLI-connid.

  WRITE:/ 'Compañía' NO-GAP,
  'Conex.' NO-GAP,
  'Desde' NO-GAP,
  'Hasta' NO-GAP,
  'Hora Sal.' NO-GAP,
  'Hora Lleg.' NO-GAP.
  SELECT carrid connid cityfrom cityto deptime arrtime INTO lt_select1
  FROM SPFLI
  WHERE carrid IN carrids AND connid IN connids.
  WRITE:/ lt_select1-carrid,
  lt_select1-connid NO-GAP,
  lt_select1-cityfrom NO-GAP,
  lt_select1-cityto NO-GAP,
  lt_select1-deptime NO-GAP,
  lt_select1-arrtime NO-GAP.
  ENDSELECT.
  
AT LINE-SELECTION.
TYPES: BEGIN OF tipo2,
fldate TYPE SFLIGHT-fldate,
price TYPE SFLIGHT-price,
currency TYPE SFLIGHT-currency,
seatsmax TYPE SFLIGHT-seatsmax,
seatsocc TYPE SFLIGHT-seatsocc,
seatsfree TYPE SFLIGHT-seatsocc,
END OF tipo2.

DATA: lt_select2 TYPE tipo2.
  CONDENSE sy-lisel .
  WRITE sy-lisel COLOR 7 NO-GAP.

  g_carrid = sy-lisel(3).
  g_connid = sy-lisel+3(4).

  ULINE.
  WRITE:
        /(20) 'FECHA' COLOR 1 NO-GAP,
         (14) 'PRECIO' COLOR 1 NO-GAP,
         (10) 'CAPACIDAD' COLOR 1 NO-GAP,
         (10) 'OCUPADAS' COLOR 1 NO-GAP,
         (10) 'LIBRES' COLOR 1 NO-GAP.
  ULINE.

SELECT fldate price currency seatsmax seatsocc
  FROM sflight
  INTO CORRESPONDING FIELDS OF lt_select2
  WHERE carrid = g_carrid AND connid = g_connid.
lt_select2-seatsfree = lt_select2-seatsmax - lt_select2-seatsocc.
WRITE:/ 
lt_select2-fldate, 
lt_select2-price, 
lt_select2-currency, 
lt_select2-seatsmax, 
lt_select2-seatsocc, 
lt_select2-seatsfree.
ENDSELECT.

Version modular

*&---------------------------------------------------------------------*
*& Report  ZDTA50K1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K1.
INCLUDE ZDTA50K1_INC.
INCLUDE ZDTA50K1_SEL.
PERFORM PRINT1.
PERFORM SELECT1.
AT LINE-SELECTION.
PERFORM LINSELECT.
PERFORM PRINT2.
PERFORM SELECT2.

INCLUDE ZDTA50K1_SUB.

subrutinas en include

*&---------------------------------------------------------------------*
*&  Include           ZDTA50K1_SUB
*&---------------------------------------------------------------------*

FORM SELECT1.
if ordline = 'X'.
      SELECT carrid connid cityfrom cityto deptime arrtime
        FROM spfli
        INTO lt_select1
        WHERE carrid IN company AND connid IN conex ORDER BY carrid.
        PERFORM PRINT3.
      ENDSELECT.
elseif ordconex = 'X'.
    SELECT carrid connid cityfrom cityto deptime arrtime
      FROM spfli
      INTO lt_select1
      WHERE carrid IN company AND connid IN conex order by connid.
      PERFORM PRINT3.
    ENDSELECT.
  endif.

ENDFORM.

FORM SELECT2.
SELECT fldate price currency seatsmax seatsocc
  FROM sflight
  INTO CORRESPONDING FIELDS OF lt_select2
  WHERE carrid = g_carrid AND connid = g_connid.
lt_select2-seatsfree = lt_select2-seatsmax - lt_select2-seatsocc.
WRITE:/
lt_select2-fldate,
lt_select2-price,
lt_select2-currency,
lt_select2-seatsmax,
lt_select2-seatsocc,
lt_select2-seatsfree.
ENDSELECT.
ENDFORM.

FORM LINSELECT.
CONDENSE sy-lisel .
WRITE sy-lisel COLOR 7 NO-GAP.
g_carrid = sy-lisel(3).
g_connid = sy-lisel+3(4).
ENDFORM.

FORM PRINT1.
WRITE:/ 'Compañía' NO-GAP,
'Conex.' NO-GAP,
'Desde' NO-GAP,
'Hasta' NO-GAP,
'Hora Sal.' NO-GAP,
'Hora Lleg.' NO-GAP.
ENDFORM.

FORM PRINT2.
ULINE.
WRITE:
      /(20) 'FECHA' COLOR 1 NO-GAP,
       (14) 'PRECIO' COLOR 1 NO-GAP,
       (10) 'CAPACIDAD' COLOR 1 NO-GAP,
       (10) 'OCUPADAS' COLOR 1 NO-GAP,
       (10) 'LIBRES' COLOR 1 NO-GAP.
ULINE.
ENDFORM.

FORM PRINT3.
  WRITE:/ lt_select1-carrid,
  lt_select1-connid NO-GAP,
  lt_select1-cityfrom NO-GAP,
  lt_select1-cityto NO-GAP,
  lt_select1-deptime NO-GAP,
  lt_select1-arrtime NO-GAP.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50K1_INC
*&---------------------------------------------------------------------*

TYPES: BEGIN OF tipo1,
  carrid TYPE SPFLI-carrid,
  connid TYPE SPFLI-connid,
  cityfrom TYPE SPFLI-cityfrom,
  cityto TYPE SPFLI-cityto,
  deptime TYPE SPFLI-deptime,
  arrtime TYPE SPFLI-arrtime,
END OF tipo1.
TYPES: BEGIN OF tipo2,
fldate TYPE SFLIGHT-fldate,
price TYPE SFLIGHT-price,
currency TYPE SFLIGHT-currency,
seatsmax TYPE SFLIGHT-seatsmax,
seatsocc TYPE SFLIGHT-seatsocc,
seatsfree TYPE SFLIGHT-seatsocc,
END OF tipo2.

DATA: lt_select1 TYPE tipo1,
      lt_select2 TYPE tipo2.
DATA: g_carrid TYPE SPFLI-carrid,
       g_connid TYPE SPFLI-connid.
	   
*&---------------------------------------------------------------------*
*&  Include           ZDTA50K1_SEL
*&---------------------------------------------------------------------*
TABLES: SPFLI.
*SELECT-OPTIONS: carrids FOR SPFLI-carrid,
*                connids FOR SPFLI-connid.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: company FOR spfli-carrid,
                conex FOR spfli-connid.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: ordline RADIOBUTTON GROUP 1 DEFAULT 'X',
            ordconex RADIOBUTTON GROUP 1.
SELECTION-SCREEN end of block b2.

modo bloque
*&---------------------------------------------------------------------*
*& Report  ZDTA50K3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K3.
TABLES: SPFLI, SFLIGHT.
SELECT-OPTIONS: company FOR spfli-carrid.

DATA: GT_DATOS TYPE STANDARD TABLE OF SPFLI,
      G_DATOS1 TYPE SPFLI,
      G_DATOS2 TYPE SFLIGHT,
      G_LINE TYPE I,
      F_PORCEN TYPE S_PRICE.

SELECT * FROM SPFLI
INTO TABLE GT_DATOS
WHERE CARRID IN company.

write:
        /(10) 'COMPAÑIA' COLOR 4 NO-GAP,"carrid
        (10) 'VUELO' COLOR 4 NO-GAP,"connid
        (10) 'SALIDA' COLOR 4 NO-GAP,"cityfrom
        (10) 'DESTINO' COLOR 4 NO-GAP,"cityto
        (15) 'HORA SALIDA' COLOR 4 NO-GAP,"deptime
        (15) 'HORA LLEGADA' COLOR 4 NO-GAP,"arrtime
        (10) 'FECHA' COLOR 4 NO-GAP,
        (10) 'CAPACIDAD' COLOR 4 NO-GAP,
        (20) 'ASIENTOS OCUPADOS' COLOR 4 NO-GAP.

G_LINE = 0.

LOOP AT GT_DATOS INTO G_DATOS1.
        G_LINE = G_LINE + 1.

        NEW-PAGE.

         WRITE: /,(10) G_DATOS1-CARRID COLOR 1 NO-GAP,"carrid
        (10) G_DATOS1-CONNID COLOR 2 NO-GAP,"connid
        (10) G_DATOS1-CITYFROM COLOR 1 NO-GAP,"cityfrom
        (10) G_DATOS1-CITYTO COLOR 2 NO-GAP,"cityto
        (15) G_DATOS1-DEPTIME COLOR 1 NO-GAP,"deptime
        (15) G_DATOS1-ARRTIME COLOR 2 NO-GAP."arrtime

        SELECT SINGLE *
        FROM SFLIGHT
        INTO G_DATOS2
        WHERE CARRID = G_DATOS1-CARRID
        AND CONNID = G_DATOS1-CONNID.

         WRITE:
        (10) G_DATOS2-FLDATE COLOR 1 NO-GAP,"carrid
        (10) G_DATOS2-SEATSMAX COLOR 2 NO-GAP,"connid
        (10) G_DATOS2-SEATSOCC COLOR 1 NO-GAP.
  ENDLOOP.
NEW-PAGE.
WRITE:/ 'TOTAL DE CONEXIONES DE VUELO MOSTRADAS: ' COLOR 3 NO-GAP,
        G_LINE  COLOR 3 NO-GAP.

AT LINE-SELECTION.

CALL FUNCTION 'Z_PORCEN81'
  EXPORTING
    imp_seatsmax       = G_DATOS2-SEATSMAX
    imp_seatsocc       = G_DATOS2-SEATSOCC
 IMPORTING
   EX_PORCEN          = F_PORCEN.

  write:
        /(10) 'COMPAÑIA' COLOR 4 NO-GAP,"carrid
        (10) 'VUELO' COLOR 4 NO-GAP,"connid
        (10) 'SALIDA' COLOR 4 NO-GAP,"cityfrom
        (10) 'DESTINO' COLOR 4 NO-GAP,"cityto
        (15) 'HORA SALIDA' COLOR 4 NO-GAP,"deptime
        (15) 'HORA LLEGADA' COLOR 4 NO-GAP,"arrtime
        (10) 'FECHA' COLOR 4 NO-GAP,
        (10) 'CAPACIDAD' COLOR 4 NO-GAP,
        (20) 'ASIENTOS OCUPADOS' COLOR 4 NO-GAP.
  WRITE: /(108) sy-lisel COLOR = 2 NO-GAP.

 WRITE: / 'Porcentaje de ocupación:' COLOR = 4 NO-GAP,
        f_porcen COLOR = 2 NO-GAP,
        '%' COLOR = 2 NO-GAP.
		
Modular

*&---------------------------------------------------------------------*
*& Report  ZDTA50K3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K3.
INCLUDE ZDTA50K3_TOP.
PERFORM SELECT1.
PERFORM WRITE1.
G_LINE = 0.

LOOP AT GT_DATOS INTO G_DATOS1.
        G_LINE = G_LINE + 1.
        PERFORM WRITE2.
        PERFORM SELECT2.
        PERFORM WRITE3.
ENDLOOP.
NEW-PAGE.
PERFORM WRITE4.
AT LINE-SELECTION.
PERFORM CALL.
PERFORM WRITE5.
INCLUDE ZDTA50K3_SEL.
INCLUDE ZWR_DATA.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50K3_TOP
*&---------------------------------------------------------------------*

TABLES: SPFLI, SFLIGHT.
SELECT-OPTIONS: company FOR spfli-carrid.

DATA: GT_DATOS TYPE STANDARD TABLE OF SPFLI,
      G_DATOS1 TYPE SPFLI,
      G_DATOS2 TYPE SFLIGHT,
      G_LINE TYPE I,
      F_PORCEN TYPE S_PRICE.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50K3_SEL
*&---------------------------------------------------------------------*

FORM SELECT1.
SELECT * FROM SPFLI
INTO TABLE GT_DATOS
WHERE CARRID IN company.
ENDFORM.

FORM SELECT2.
SELECT SINGLE *
FROM SFLIGHT
INTO G_DATOS2
WHERE CARRID = G_DATOS1-CARRID
AND CONNID = G_DATOS1-CONNID.
ENDFORM.

FORM CALL.
CALL FUNCTION 'Z_PORCEN81'
  EXPORTING
    imp_seatsmax       = G_DATOS2-SEATSMAX
    imp_seatsocc       = G_DATOS2-SEATSOCC
 IMPORTING
   EX_PORCEN          = F_PORCEN.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZWR_DATA
*&---------------------------------------------------------------------*
FORM WRITE1.
write:
        /(10) 'COMPAÑIA' COLOR 4 NO-GAP,"carrid
        (10) 'VUELO' COLOR 4 NO-GAP,"connid
        (10) 'SALIDA' COLOR 4 NO-GAP,"cityfrom
        (10) 'DESTINO' COLOR 4 NO-GAP,"cityto
        (15) 'HORA SALIDA' COLOR 4 NO-GAP,"deptime
        (15) 'HORA LLEGADA' COLOR 4 NO-GAP,"arrtime
        (10) 'FECHA' COLOR 4 NO-GAP,
        (10) 'CAPACIDAD' COLOR 4 NO-GAP,
        (20) 'ASIENTOS OCUPADOS' COLOR 4 NO-GAP.
ENDFORM.

FORM WRITE2.
WRITE: /,(10) G_DATOS1-CARRID COLOR 1 NO-GAP,"carrid
(10) G_DATOS1-CONNID COLOR 2 NO-GAP,"connid
(10) G_DATOS1-CITYFROM COLOR 1 NO-GAP,"cityfrom
(10) G_DATOS1-CITYTO COLOR 2 NO-GAP,"cityto
(15) G_DATOS1-DEPTIME COLOR 1 NO-GAP,"deptime
(15) G_DATOS1-ARRTIME COLOR 2 NO-GAP."arrtime
ENDFORM.

FORM WRITE3.
WRITE:
(10) G_DATOS2-FLDATE COLOR 1 NO-GAP,"carrid
(10) G_DATOS2-SEATSMAX COLOR 2 NO-GAP,"connid
(10) G_DATOS2-SEATSOCC COLOR 1 NO-GAP.
ENDFORM.

FORM WRITE4.
WRITE:/ 'TOTAL DE CONEXIONES DE VUELO MOSTRADAS: ' COLOR 3 NO-GAP,
        G_LINE  COLOR 3 NO-GAP.
ENDFORM.

FORM WRITE5.
  write:
        /(10) 'COMPAÑIA' COLOR 4 NO-GAP,"carrid
        (10) 'VUELO' COLOR 4 NO-GAP,"connid
        (10) 'SALIDA' COLOR 4 NO-GAP,"cityfrom
        (10) 'DESTINO' COLOR 4 NO-GAP,"cityto
        (15) 'HORA SALIDA' COLOR 4 NO-GAP,"deptime
        (15) 'HORA LLEGADA' COLOR 4 NO-GAP,"arrtime
        (10) 'FECHA' COLOR 4 NO-GAP,
        (10) 'CAPACIDAD' COLOR 4 NO-GAP,
        (20) 'ASIENTOS OCUPADOS' COLOR 4 NO-GAP.
  WRITE: /(108) sy-lisel COLOR = 2 NO-GAP.

 WRITE: / 'Porcentaje de ocupación:' COLOR = 4 NO-GAP,
        f_porcen COLOR = 2 NO-GAP,
        '%' COLOR = 2 NO-GAP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Report  ZDTA50K4
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50K4.
TABLES : ZAREAS, ZALMACEN00.

DATA gt_datos TYPE STANDARD TABLE OF zalmacen00.
DATA g_datos TYPE zareas.
DATA g_id TYPE zidentalma.
SELECT-OPTIONS: AREAS FOR zalmacen00-area.
PARAMETERS: p_si RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_no RADIOBUTTON GROUP rad1.
  SELECT *
    FROM zalmacen00
    INTO TABLE gt_datos
    WHERE area IN AREAS.

  IF sy-subrc NE 0.
    WRITE: / 'La búsqueda no produjo resultados.' COLOR = 6 NO-GAP.
  ENDIF.
  DATA f_datos TYPE zalmacen00.

  WRITE: /(10) 'ID' COLOR = 1 NO-GAP,
        (30) 'Nombre' COLOR = 1 NO-GAP,
        (10) 'Tipo' COLOR = 1 NO-GAP,
        (10) 'Centro' COLOR = 1 NO-GAP,
        (10) 'Alm. SAP' COLOR = 1 NO-GAP.

  LOOP AT gt_datos INTO f_datos.

    WRITE: /(10) f_datos-ident COLOR = 4 NO-GAP,
    (30) f_datos-nombre COLOR = 2 NO-GAP,
    (10) f_datos-tipo COLOR = 2 NO-GAP,
    (10) f_datos-centro COLOR = 2 NO-GAP,
    (10) f_datos-alma_sap COLOR = 2 NO-GAP.

  ENDLOOP.
AT LINE-SELECTION.
  IF p_si = 'X'.

    WRITE: /(10) 'ID' COLOR = 1 NO-GAP,
          (30) 'Nombre' COLOR = 1 NO-GAP,
          (10) 'Tipo' COLOR = 1 NO-GAP,
          (10) 'Centro' COLOR = 1 NO-GAP,
          (10) 'Alm. SAP' COLOR = 1 NO-GAP.

    WRITE: /(70) sy-lisel COLOR = 7 NO-GAP.

    NEW-PAGE.

    WRITE: /(10) 'Centro' COLOR = 1 NO-GAP,
            (25) 'Descripción' COLOR = 1 NO-GAP,
            (10) 'Almacén' COLOR = 1 NO-GAP,
            (25) 'Descripción' COLOR = 1 NO-GAP.

             g_id = sy-lisel+50(4).

    SELECT SINGLE *
    FROM zareas
    INTO g_datos
    WHERE area = g_id.

    WRITE: /(10) g_datos-area COLOR = 2 NO-GAP,
          (25) g_datos-descripcion COLOR = 2 NO-GAP.

    g_id = sy-lisel+60(4).

    SELECT SINGLE *
    FROM zareas
    INTO g_datos
    WHERE area = g_id.

    WRITE: (10) g_datos-area COLOR = 2 NO-GAP,
           (25) g_datos-descripcion COLOR = 2 NO-GAP.

  ELSEIF p_no = 'X'.
    MESSAGE 'Detalles desactivados.' TYPE 'I'.
    LEAVE TO SCREEN 0.
  ENDIF.
  
*  EJEMPLO ALV OBJETOS
  
*&---------------------------------------------------------------------*
*& Report  ZDTA81I1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdta81i1.
include zdta81i1_inc_top.

include zdta81i1_inc_sel.

START-OF-SELECTION.

  SELECT * FROM sflight
    INTO CORRESPONDING FIELDS OF TABLE t1
  WHERE ( carrid IN company ) AND ( connid IN conexion ) AND ( fldate IN fecha ).

END-OF-SELECTION.
  CALL SCREEN 0200.



  LOOP AT t1 INTO t2.
  ENDLOOP.

*******************************************************
*Definición de la clase destinada a manejar los eventos
*******************************************************


*******************************************************
*MONTAR CATALOGO
****************************************************
INCLUDE zdta81i1_inc_catalogo.


INCLUDE zdta81i1bo."INCLUDE DYNPRO.

INCLUDE zdta81i1ai. "INCLUDES DYNPRO

*&---------------------------------------------------------------------*
*&  Include           ZDTA81I1_INC_TOP
*&---------------------------------------------------------------------*
CLASS alv_eventos DEFINITION.
  PUBLIC SECTION .
    METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
            IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "alv_eventos DEFINITION

*----------------------------------------------------------------------*
*       CLASS alv_eventos IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS alv_eventos IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "alv_eventos IMPLEMENTATION
*DATA       g_event_handler TYPE REF TO alv_eventos.
TABLES sflight.

DATA: t2 TYPE sflight."ESTRUCTURA
DATA: t1 TYPE TABLE OF sflight. "TABLA INTERNA

*******************************************************
*VARIABLES DEL OBJETO PARA EL ALV
*******************************************************
DATA:
      gt_fieldcat TYPE lvc_t_fcat,"CATALOGO
      g_layout TYPE lvc_s_layo,"LAYOUT
      go_conte TYPE REF TO cl_gui_custom_container,"CONTENEDOR
      go_alvgr TYPE REF TO cl_gui_alv_grid,"OBJETO ALV
      go_sname TYPE scrfname. "Nombre que mostrará el ALV.
"Tendrá que generarse de forma dinámica

DATA g_event_handler TYPE REF TO alv_eventos.

*&---------------------------------------------------------------------*
*&  Include           ZDTA81I1_INC_SEL
*&---------------------------------------------------------------------*
*PANTALLA DE SELECCION

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:
company FOR sflight-carrid,
conexion FOR sflight-connid,
fecha FOR sflight-fldate.
SELECTION-SCREEN END OF BLOCK b1.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
*PARAMETERS:
**precio TYPE sflight-price,
*precio AS CHECKBOX DEFAULT ' '.
**moneda AS CHECKBOX DEFAULT ' ',
**avion AS CHECKBOX DEFAULT '
**ocmaxeco AS CHECKBOX DEFAULT ' ',
**resereco AS CHECKBOX DEFAULT ' ',
**resertot AS CHECKBOX DEFAULT ' ',
**ocmaxbus AS CHECKBOX DEFAULT ' ',
**reserbus AS CHECKBOX DEFAULT ' ',
**ocmaxpr AS CHECKBOX DEFAULT ' ',
**resprim AS CHECKBOX DEFAULT ' '.
*SELECTION-SCREEN END OF BLOCK b2.

PROCESS BEFORE OUTPUT.
* MODULE STATUS_0200.
*
MODULE CARGAR_ALV.
PROCESS AFTER INPUT.
MODULE EXIT_COMMAND AT EXIT-COMMAND.
* MODULE USER_COMMAND_0200.

*&---------------------------------------------------------------------*
*&  Include           ZDTA81I1_INC_CATALOGO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*llamada a subrutinas



*Rutina hotspot_click, que es invocada desde el método handle_hotspot_click********************************
FORM hotspot_click  USING p_fila TYPE lvc_s_row  "índice de la fila seleccionada
                          p_columna TYPE lvc_s_col "el campo p_columna-fieldname contiene el nombre de la columna que se ha pulsado
                          p_row TYPE lvc_s_roid.
  DATA: f_lin TYPE sflight. "Existirá una tabla sin cabecera llamada GT_LIN donde tenemos todos los datos que se muestran en el alv, la
  "variable f_lin es del mismo tipo que GT_LIN y actúa como cabecera de la tabla

  READ TABLE t1 INTO f_lin INDEX p_row-row_id.
  "Leemos la tabla en la que tenemos los datos con el índice de la fila pulsada
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  IF p_columna-fieldname EQ 'COLUMNA1'.
*Aquí se pondrán las acciones que queremos que se realicen cuando se hace click sobre el campo COLUMNA1 de una línea del ALV

  ELSEIF p_columna-fieldname EQ 'COLUMNA2'.
*Aquí se pondrán las acciones que queremos que se realicen cuando se hace click sobre el campo COLUMNA2 de una línea del ALV
    .
    .
    .
    .
    .
    .
  ENDIF.

ENDFORM.                    " HOTSPOT_CLICK
***********************************************************
*MONTAR LAYOUT
***********************************************************

*Definición del layout (el aspecto general del ALV)***************************************
FORM montar_layout ." llamada en el dynpro
  CLEAR: g_layout.

*CREAR LAYOUT
  g_layout-zebra = 'X' .
  g_layout-sel_mode   = 'A'.
  g_layout-cwidth_opt = 'X'.
  g_layout-col_opt = 'X'.
  g_layout-grid_title = go_sname.

ENDFORM.                    " MONTAR_LAYOUT
*******************************************************
*MONTAR CATALOGO
****************************************************
FORM catalog0200 .


  DATA: ls_fieldcatalog TYPE lvc_s_fcat. "Campo que actua de cabecera para GT_FIELDCAT,

  CHECK gt_fieldcat[] IS INITIAL.
***MONTAR 13 COLUMNAS
  ls_fieldcatalog-col_pos = '1'. "Posición que ocupa el campo cuando se visualice el listado (el número de columna)
  ls_fieldcatalog-fieldname = 'CARRID'. "Nombre del campo dentro de la tabla interna que contiene los datos a mostrar
  ls_fieldcatalog-tabname = 'T1'. "Nombre de la tabla interna que contiene los datos a mostrar
  ls_fieldcatalog-coltext  = 'LINEA AEREA'. "Texto que encabezará la columna en la que aparece el campo
  ls_fieldcatalog-just = 'R'. "Justificación: L--> Izquierda, C --> Centrado, R --> Derecha
*  ls_fieldcatalog-no_out = 'X'. "Si queremos que el campo esté oculto rellenamos este valor con 'X'
  ls_fieldcatalog-outputlen = '20'. "Longitud con la que se mostrará el campo
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '2'.
  ls_fieldcatalog-fieldname = 'CONNID'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'CONEXION'.
*ls_fieldcatalog-key = 'X'.
  ls_fieldcatalog-just = 'R'.
*  ls_fieldcatalog-no_out = 'X'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '3'.
  ls_fieldcatalog-fieldname = 'FLDATE'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'FECHA VUELO'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.


  ls_fieldcatalog-col_pos = '4'.
  ls_fieldcatalog-fieldname = 'PRICE'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'PRECIO VUELO'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.


  ls_fieldcatalog-col_pos = '5'.
  ls_fieldcatalog-fieldname = 'CURRENCY'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'MONEDA LOCAL'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '6'.
  ls_fieldcatalog-fieldname = 'PLANETYPE'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'TIPO DE AVION'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '7'.
  ls_fieldcatalog-fieldname = 'SEATSMAX'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'OC MAX CLAS ECON'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '8'.
  ls_fieldcatalog-fieldname = 'SEATSOCC'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'RESERVAS CLAS ECON'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '9'.
  ls_fieldcatalog-fieldname = 'PAYMENTSUM'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'TOTAL RESERVAS'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '10'.
  ls_fieldcatalog-fieldname = 'SEATSMAX_B'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'OC MAX CLAS BUSS'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.


  ls_fieldcatalog-col_pos = '11'.
  ls_fieldcatalog-fieldname = 'SEATSOCC_B'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'RES CLAS BUSS'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  ls_fieldcatalog-col_pos = '12'.
  ls_fieldcatalog-fieldname = 'SEATSMAX_F'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'OC MAX FIRST CLAS '.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.


  ls_fieldcatalog-col_pos = '13'.
  ls_fieldcatalog-fieldname = 'SEATSOCC_F'.
  ls_fieldcatalog-tabname = 'T1'.
  ls_fieldcatalog-coltext  = 'RES FIRST CLAS'.
  ls_fieldcatalog-just = 'R'.
  ls_fieldcatalog-outputlen = '20'.
  APPEND  ls_fieldcatalog TO gt_fieldcat.
  CLEAR ls_fieldcatalog.

  .
  .... "Tantas veces como campos queramos mostrar. Se queda más bonito si los valores para ls_fieldcatalog se pasan por perform




ENDFORM.                    " CATALOGO_0100

**********************************************
*Montar el ALV
****************************************************

FORM montar_alv.
  "Creamos el contenedor donde se mostrará el ALV
  IF go_conte IS INITIAL.

    CREATE OBJECT go_conte "Creamos el contenedor del alv
      EXPORTING
        container_name = 'LISTADO'. "Nombre que le hemos dado dentro del dynpro

    CREATE OBJECT go_alvgr "Creamos el objeto ALV
      EXPORTING
        i_parent = go_conte.

*Creamos el manejador de eventos para el HotSpot
    CREATE OBJECT g_event_handler.

*Le decimos al manejador de eventos que va a capturar los eventos del ALV
    SET HANDLER g_event_handler->handle_hotspot_click FOR go_alvgr.
  ENDIF.

***************************
  "VARIANTE DE VISUALIZACION
****************************

  DATA: l_variant TYPE disvariant.
  CONSTANTS: c_variant TYPE disvariant-variant VALUE '/PREDETERM'.

  l_variant-report = sy-repid.
  l_variant-variant = c_variant.

*LLamamos al método que crea el ALV
  CALL METHOD go_alvgr->set_table_for_first_display
    EXPORTING
      i_save     = 'A'
      is_layout  = g_layout "El layout que hemos creado
      is_variant = l_variant
  changing
    it_outtab       = t1 "Tabla interna con los datos a mostrar
    it_fieldcatalog = gt_fieldcat. "Catálogo creado


ENDFORM.                   " MONTAR_ALV

*----------------------------------------------------------------------*
***INCLUDE ZDTA81I1BO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CARGAR_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CARGAR_ALV OUTPUT.
PERFORM MONTAR_LAYOUT.
PERFORM CATALOG0200.
PERFORM MONTAR_ALV.
ENDMODULE.                 " CARGAR_ALV  OUTPUT

*----------------------------------------------------------------------*
***INCLUDE ZDTA81I1AI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND INPUT.
LEAVE TO SCREEN 0. " SALTE DEL PROGRAM
ENDMODULE.                 " EXIT_COMMAND  INPUT


*Primer ALV
*&---------------------------------------------------------------------*
*& Report  ZDTA50I1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdta50i1.
TABLES: sflight.
SELECT-OPTIONS: company FOR sflight-carrid,
                conn FOR sflight-connid,
                date FOR sflight-fldate.
DATA: g_sflight TYPE TABLE OF sflight.
SELECT * INTO TABLE g_sflight FROM sflight
WHERE carrid IN company AND connid IN conn AND fldate IN date.

CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
exporting
  i_structure_name = 'SFLIGHT'
  i_callback_program = 'SY-REPID'
  tables
    t_outtab = g_sflight.
	
*&---------------------------------------------------------------------*
*& Report  ZDTA50I1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdta50i1.
TABLES: sflight.
TYPE-POOLS: slis.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: company FOR sflight-carrid,
                conn FOR sflight-connid,
                date FOR sflight-fldate.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS : simple RADIOBUTTON GROUP RB1,
complex RADIOBUTTON GROUP RB1.
SELECTION-SCREEN END OF BLOCK b2.
IF simple = 'X'.
TYPES: BEGIN OF TIPO,
  CARRID TYPE SFLIGHT-CARRID,
  CONNID TYPE SFLIGHT-CONNID,
  FLDATE TYPE SFLIGHT-FLDATE,
  PRICE TYPE SFLIGHT-PRICE,
  CURRENCY TYPE SFLIGHT-CURRENCY,
  PLANETYPE TYPE SFLIGHT-PLANETYPE,
  SEATSMAX TYPE SFLIGHT-SEATSMAX,
  SEATSOCC TYPE SFLIGHT-SEATSOCC,
  PAYMENTSUM TYPE SFLIGHT-PAYMENTSUM,
END OF TIPO.
DATA: GT_SFLIGHT TYPE TABLE OF TIPO,
      it_fieldcat  TYPE slis_t_fieldcat_alv,
      wa_fieldcat  TYPE slis_fieldcat_alv.
SELECT CARRID CONNID FLDATE PRICE CURRENCY PLANETYPE SEATSMAX SEATSOCC PAYMENTSUM
  INTO TABLE GT_SFLIGHT FROM SFLIGHT
  WHERE carrid IN company AND connid IN conn AND fldate IN date.
  
wa_fieldcat-fieldname  = 'CARRID'.
wa_fieldcat-seltext_m  = 'Comp. No.'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'CONNID'.
wa_fieldcat-seltext_m  = 'Con. No.'.
APPEND wa_fieldcat TO it_fieldcat.
  
wa_fieldcat-fieldname  = 'FLDATE'.
wa_fieldcat-seltext_m  = 'Fecha vuelo'.
APPEND wa_fieldcat TO it_fieldcat.
  
wa_fieldcat-fieldname  = 'PRICE'.
wa_fieldcat-seltext_m  = 'Precio'.
APPEND wa_fieldcat TO it_fieldcat.
  
wa_fieldcat-fieldname  = 'CURRENCY'.
wa_fieldcat-seltext_m  = 'moneda'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'PLANETYPE'.
wa_fieldcat-seltext_m  = 'Tipo vuelo'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'SEATSMAX'.
wa_fieldcat-seltext_m  = 'Pl. Max.'.
APPEND wa_fieldcat TO it_fieldcat.
  
wa_fieldcat-fieldname  = 'SEATSOCC'.
wa_fieldcat-seltext_m  = 'Pl. Ocup.'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'PAYMENTSUM'.
wa_fieldcat-seltext_m  = 'Suma'.
APPEND wa_fieldcat TO it_fieldcat.
  
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    it_fieldcat   = it_fieldcat
  TABLES
    t_outtab      = gt_sflight
  EXCEPTIONS
    program_error = 1
    OTHERS        = 2.
ELSEIF complex = 'X'.
DATA: g_sflight TYPE TABLE OF sflight.
SELECT * INTO TABLE g_sflight FROM sflight
WHERE carrid IN company AND connid IN conn AND fldate IN date.
CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
exporting
  i_structure_name = 'SFLIGHT'
  i_callback_program = 'SY-REPID'
  tables
    t_outtab = g_sflight.
ENDIF.

*&---------------------------------------------------------------------*
*& Report  ZDTA50I2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50I2.
INCLUDE ZDTA50I2_DATA.

AT SELECTION-SCREEN.
INCLUDE ZDTA50I2_SEL.
START-OF-SELECTION.
PERFORM MOSTRAR_ALV.

INCLUDE ZDTA50I2_FORM.
INCLUDE ZDTA50I2_CAT.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50I2_DATA
*&---------------------------------------------------------------------*
TABLES: SPFLI, SFLIGHT.
TYPE-POOLS slis.

TYPES:  BEGIN OF tipo,"TABLA SFLIGHT
        carrid TYPE sflight-carrid,
        connid TYPE sflight-connid,
        fldate TYPE sflight-fldate,
        price TYPE sflight-price,
        currency TYPE sflight-currency,
        planetype TYPE sflight-planetype,
        seatsmax TYPE sflight-seatsmax,
        seatsocc TYPE sflight-seatsocc,
        paymentsum TYPE sflight-paymentsum,
        END OF tipo.

DATA: G_SPFLI TYPE TABLE OF SPFLI,
      G_SFLIGHT TYPE TABLE OF tipo WITH HEADER LINE,
      WA_SPFLI LIKE LINE OF G_SPFLI,
      WA_SFLIGHT LIKE LINE OF G_SFLIGHT.

DATA: gk_si TYPE c VALUE 'X',
      gk_no TYPE c VALUE ' '.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
company FOR sflight-carrid,
conexion FOR sflight-connid.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
fecha FOR sflight-fldate,
tipo FOR sflight-planetype.
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50I2_SEL
*&---------------------------------------------------------------------*
SELECT * INTO TABLE g_spfli FROM spfli
WHERE carrid IN company AND connid IN conexion.
SELECT
  CARRID
  CONNID
  FLDATE
  PRICE
  CURRENCY
  PLANETYPE
  SEATSMAX
  SEATSOCC
  PAYMENTSUM
  INTO TABLE g_sflight FROM sflight
WHERE fldate IN fecha AND planetype IN tipo.
SORT g_spfli.

*&---------------------------------------------------------------------*
*&  Include           ZDTA50I2_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ALV
*&---------------------------------------------------------------------*
FORM mostrar_alv.

  DATA: f_keyinfo TYPE slis_keyinfo_alv,
        t_catalogo TYPE slis_t_fieldcat_alv,
        f_layo TYPE slis_layout_alv,
        t_events TYPE slis_t_event,
        f_event TYPE slis_alv_event,
        t_exits TYPE slis_t_event_exit,
        f_exits TYPE slis_event_exit.

  CLEAR: f_layo, f_keyinfo, f_event, f_exits.
  FREE: t_events, t_catalogo, t_exits.

  PERFORM layout CHANGING f_layo.
  PERFORM jerarquia CHANGING f_keyinfo.
  PERFORM catalogo CHANGING t_catalogo.

*Asignamos el rutina del user comand a la de eventos
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = t_events[]
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  LOOP AT t_events INTO f_event.
    IF f_event-name = 'USER_COMMAND'.
      f_event-form = 'USER_COMMAND_ALV'.
    ELSE.
      CONTINUE.
    ENDIF.
    MODIFY t_events FROM f_event.
  ENDLOOP.

  f_exits-ucomm = '&F03'.
  f_exits-after = gk_si.
  f_exits-before = gk_si.
  APPEND f_exits TO t_exits.
  f_exits-ucomm = '&F15'.
  APPEND f_exits TO t_exits.
  f_exits-ucomm = '&F12'.
  APPEND f_exits TO t_exits.

*Invocamos el alv
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program             = sy-repid
      i_callback_user_command        = 'USER_COMMAND_ALV'
      is_layout                      = f_layo
      it_fieldcat                    = t_catalogo[]
      it_events                      = t_events[]
      it_event_exit                  = t_exits[]
      i_tabname_header               = 'T1'
      i_tabname_item                 = 'T_SF'
      is_keyinfo                     = f_keyinfo
    TABLES
      t_outtab_header                = G_SPFLI
      t_outtab_item                  = G_SFLIGHT
              .

ENDFORM.                    " MOSTRAR_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
FORM layout  CHANGING  p_layout TYPE slis_layout_alv.
  p_layout-expand_fieldname = 'CARRID'.
  p_layout-expand_all = gk_si.
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  JERARQUIA
*&---------------------------------------------------------------------*
FORM jerarquia  CHANGING p_keyinfo TYPE slis_keyinfo_alv.

  p_keyinfo-header01 = 'CARRID'.
  p_keyinfo-header02 = 'CONNID'.
  p_keyinfo-item01 = 'CARRID'.
  p_keyinfo-item02 = 'CONNID'.

ENDFORM.                    " JERARQUIA

*&---------------------------------------------------------------------*
*&      Form  STATUS_ALV
*&---------------------------------------------------------------------*
FORM status_alv USING p_extab TYPE slis_t_extab.

  TYPES BEGIN OF ty_excluidos.
  TYPES fcode TYPE rsmpe-func.
  TYPES END OF ty_excluidos.
  DATA: t_excluidos TYPE STANDARD TABLE OF ty_excluidos,
        f_excluidos TYPE ty_excluidos.

  CLEAR: f_excluidos.
  FREE: t_excluidos.

  f_excluidos-fcode = '&EB9'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&ILT'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&UMC'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&SUM'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&RNT_PREV'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&AQW'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '%PC'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '%SL'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&ABC'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&GRAPH'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&OL0'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&OAD'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&AVE'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&ALL'.
  APPEND f_excluidos TO t_excluidos.
  f_excluidos-fcode = '&SAL'.
  APPEND f_excluidos TO t_excluidos.

  SET PF-STATUS 'STT_0001' EXCLUDING t_excluidos.
ENDFORM.                    "status_alv
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_ALV
*&---------------------------------------------------------------------*
FORM user_command_alv USING p_ucomm TYPE sy-ucomm
                            p_selfield TYPE slis_selfield.
  DATA: f_ucomm TYPE sy-ucomm.

  f_ucomm = p_ucomm.
  IF f_ucomm = '&IC1'.
    IF p_selfield-fieldname = 'CARRID' OR p_selfield-fieldname = 'CONNID' OR p_selfield-fieldname = 'FLDATE'.
      CHECK NOT p_selfield-value IS INITIAL.
      PERFORM mostrar USING p_selfield-tabindex
            .
    ENDIF.
  ENDIF.
ENDFORM.                    "user_command_alv
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SELFIELD_VALUE  text
*----------------------------------------------------------------------*
FORM mostrar  USING   asientos TYPE  sy-tabix.
  " INDICE
  DATA: str TYPE c LENGTH 10,
        wa_sflight2 LIKE LINE OF g_sflight.
  READ TABLE G_SFLIGHT INDEX asientos. " LEO TABLA INTERNA COMO ES CON CABECERA LA PROPIA

  WRITE: G_SFLIGHT-seatsocc TO str .
CONDENSE STR NO-GAPS.
  CONCATENATE 'plazas' str INTO str SEPARATED BY SPACE.
  MESSAGE str TYPE 'I'.
ENDFORM.


*&---------------------------------------------------------------------*
*& Modulpool         SAPMZ501
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE MZ501TOP                                .    " global Data

INCLUDE MZ501O01                                .    " PBO-Modules
INCLUDE MZ501I01                                .    " PAI-Modules
INCLUDE MZ501F01                                .    " FORM-Routines

*&---------------------------------------------------------------------*
*& Include MZ501TOP                                          Modulpool        SAPMZ501
*&
*&---------------------------------------------------------------------*

PROGRAM  sapmz501 MESSAGE-ID zoe_ts.

TABLES sflight.

DATA: ok_code LIKE sy-ucomm.

*&---------------------------------------------------------------------*
*&  Include           MZ501O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE stt_0100 OUTPUT.
  SET PF-STATUS 'STT_0100'.
  SET TITLEBAR 'TIT_0100'.
ENDMODULE.                 " STT_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STT_0200'.
  SET TITLEBAR 'TIT_0200'.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&  Include           MZ501I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SELE'.
      SELECT SINGLE * FROM sflight
        WHERE carrid = sflight-carrid AND
              connid = sflight-connid AND
              fldate = sflight-fldate.
      IF sy-subrc = 0.
        LEAVE TO SCREEN 0200.
      ELSE.
        MESSAGE i001 WITH 'No existen vuelos con las conidciones dadas.'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE ok_code.
    WHEN 'ENDC'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR sflight.
      LEAVE TO SCREEN 0100.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&  Include           MZ501F01
*&---------------------------------------------------------------------*


https://www.youtube.com/watch?v=bT5L6N7HFyw
https://www.youtube.com/watch?v=bxnw-DUXeXU
https://www.youtube.com/watch?v=JzmSAPgUbU0
https://www.youtube.com/watch?v=0TMMdbY9lJ4

*&---------------------------------------------------------------------*
*& Modulpool         SAPMZ501_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE MZ501_1TOP                              .    " global Data

 INCLUDE MZ501_1O01                              .  " PBO-Modules
 INCLUDE MZ501_1I01                              .  " PAI-Modules
 INCLUDE MZ501_1F01                              .  " FORM-Routines
 
 *&---------------------------------------------------------------------*
*& Include MZ501_1TOP                                        Modulpool        SAPMZ501_1
*&
*&---------------------------------------------------------------------*

PROGRAM  SAPMZ501_1.

DATA: company TYPE SFLIGHT-CARRID,
      conexion TYPE SFLIGHT-CONNID,
      fecha TYPE SFLIGHT-FLDATE,
      precio TYPE SFLIGHT-PRICE,
      moneda TYPE SFLIGHT-CURRENCY,
      tipo TYPE SFLIGHT-PLANETYPE,
      capacidad TYPE SFLIGHT-SEATSMAX,
      ocupados TYPE SFLIGHT-SEATSOCC,
      suma TYPE SFLIGHT-PAYMENTSUM,
      capacidad_b TYPE SFLIGHT-SEATSMAX_B,
      ocupados_b TYPE SFLIGHT-SEATSOCC_B,
      capacidad_f TYPE SFLIGHT-SEATSMAX_F,
      ocupados_f TYPE SFLIGHT-SEATSOCC_F,
      ok_code TYPE SY-UCOMM.
DATA: G_SFLIGHT TYPE TABLE OF SFLIGHT WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&  Include           MZ501_1O01
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
SET PF-STATUS 'STATUS100'.
SET TITLEBAR 'TITLE100'.
ENDMODULE.
MODULE STATUS_0200 OUTPUT.
SET PF-STATUS 'STATUS200'.
SET TITLEBAR 'TITLE200'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           MZ501_1I01
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
CASE ok_code.
WHEN 'BUSCAR'.
SELECT * INTO TABLE G_SFLIGHT
  FROM SFLIGHT
  WHERE
  CARRID = company
  AND CONNID = conexion
  AND FLDATE = fecha.
  LOOP AT G_SFLIGHT.
    company = G_SFLIGHT-CARRID.
    conexion = G_SFLIGHT-CONNID.
    fecha = G_SFLIGHT-FLDATE.
    precio = G_SFLIGHT-PRICE.
    moneda = G_SFLIGHT-CURRENCY.
    tipo = G_SFLIGHT-PLANETYPE.
    capacidad = G_SFLIGHT-SEATSMAX.
    ocupados = G_SFLIGHT-SEATSOCC.
    suma = G_SFLIGHT-PAYMENTSUM.
    capacidad_b = G_SFLIGHT-SEATSMAX_B.
    ocupados_b = G_SFLIGHT-SEATSOCC_B.
    capacidad_f = G_SFLIGHT-SEATSMAX_F.
    ocupados_f = G_SFLIGHT-SEATSOCC_F.
  ENDLOOP.
  IF SY-SUBRC = 0.
    LEAVE TO SCREEN 0200.
  ELSE.
    WRITE:/ 'Error de parámetros'.
  ENDIF.
WHEN 'ATRAS' OR 'BACK'.
  LEAVE PROGRAM.
ENDCASE.
ENDMODULE.
MODULE USER_COMMAND_0200 INPUT.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           MZ501_1F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Modulpool         SAPMZ505
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE MZ505TOP                                .    " global Data

 INCLUDE MZ505O01                                .  " PBO-Modules
 INCLUDE MZ505I01                                .  " PAI-Modules
 INCLUDE MZ505F01 
 
 *&---------------------------------------------------------------------*
*& Include MZ505TOP                                          Modulpool        SAPMZ505
*&
*&---------------------------------------------------------------------*

PROGRAM  SAPMZ505.

DATA: P_IDENT TYPE ZALMACEN50-IDENT,
      P_NOMBRE TYPE ZALMACEN50-NOMBRE,
      P_AREA TYPE ZALMACEN50-AREA,
      P_TIPO TYPE ZALMACEN50-TIPO,
      P_CENTRO TYPE ZALMACEN50-CENTRO,
      P_ALMA_SAP TYPE ZALMACEN00-ALMA_SAP,
      P_BNAME TYPE ZALMACEN50-BNAME,
      P_FEMOD TYPE ZALMACEN50-FEMOD,
      OK_CODE TYPE SY-UCOMM,
      OK_CODE2 TYPE SY-UCOMM.
DATA: G_ZALMACEN50 TYPE TABLE OF ZALMACEN50 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&  Include           MZ505O01
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS200'.
  SET TITLEBAR 'TITLE200'.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&  Include           MZ505I01
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
CASE OK_CODE.
  WHEN 'BUSCAR'.
    SELECT * INTO TABLE G_ZALMACEN50
      FROM ZALMACEN50
      WHERE IDENT = P_IDENT.
    LOOP AT G_ZALMACEN50.
      P_IDENT = G_ZALMACEN50-IDENT.
      P_NOMBRE = G_ZALMACEN50-NOMBRE.
      P_TIPO = G_ZALMACEN50-TIPO.
      P_AREA = G_ZALMACEN50-AREA.
      P_CENTRO = G_ZALMACEN50-CENTRO.
      P_ALMA_SAP = G_ZALMACEN50-ALMA_SAP.
      P_BNAME = G_ZALMACEN50-BNAME.
      P_FEMOD = G_ZALMACEN50-FEMOD.
    ENDLOOP.
      IF SY-SUBRC = 0.
    LEAVE TO SCREEN 0200.
  ELSE.
    WRITE:/ 'Error de parámetros'.
  ENDIF.
ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
MODULE USER_COMMAND_0200 INPUT.
CASE OK_CODE.
  WHEN 'GUARDAR'.
    UPDATE ZALMACEN50
    SET ident = P_IDENT
        nombre = P_NOMBRE
        area = P_AREA
        tipo = P_TIPO
        centro = P_CENTRO
        alma_sap = P_ALMA_SAP
        bname = P_BNAME
        femod = P_FEMOD
    WHERE ident = P_IDENT.
 ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&  Include           MZ505F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Report  ZACTIVIDAD_DEPURACION50
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZACTIVIDAD_DEPURACION50.

TABLES kna1.

DATA: ft_kna1 LIKE kna1 OCCURS 0 WITH HEADER LINE.

PARAMETERS: pa_reg LIKE kna1-regio.

CLEAR ft_kna1.
REFRESH ft_kna1.

SELECT * FROM kna1 INTO CORRESPONDING FIELDS OF TABLE ft_kna1
  WHERE regio = pa_reg.

WRITE:/ 'DEUDOR             NOMBRE                            PAIS              CALLE                    PROVINCIA    POBLACION'.
ULINE.
LOOP AT ft_kna1.
  WRITE:/ ft_kna1-kunnr, '||',
          ft_kna1-name1, '||',
          ft_kna1-land1, '||',
          ft_kna1-stras, '||',
          ft_kna1-regio, '||',
          ft_kna1-ort01.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Report  ZACTIVIDAD_DEPURACION50
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZACTIVIDAD_DEPURACION50_1.

TABLES kna1.

DATA: ft_kna1 LIKE kna1 OCCURS 0 WITH HEADER LINE.

PARAMETERS: pa_reg LIKE kna1-regio.

WRITE:/ 'DEUDOR             NOMBRE                            PAIS              CALLE                    PROVINCIA    POBLACION'.
ULINE.
SELECT * FROM kna1
  WHERE regio = pa_reg.
  WRITE:/ kna1-kunnr, '||',
          kna1-name1, '||',
          kna1-land1, '||',
          kna1-stras, '||',
          kna1-regio, '||',
          kna1-ort01.
ENDSELECT.


*&---------------------------------------------------------------------*
*& Report  ZDTA50L1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50L1.
DATA: G_ZALMACEN TYPE TABLE OF ZALMACEN50.
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename                      = 'C:\tmp\ejercicio 32\Almacenes.txt'
    filetype                      = 'ASC'
  TABLES
    data_tab                      = G_ZALMACEN.
MODIFY zalmacen50 FROM TABLE G_ZALMACEN.

*&---------------------------------------------------------------------*
*& Report  ZDTA50L1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50L1.
PARAMETERS : Upload RADIOBUTTON GROUP RB1,
Download RADIOBUTTON GROUP RB1.
DATA: G_ZALMACEN TYPE TABLE OF ZALMACEN50.
IF Upload = 'X'.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = 'C:\tmp\ejercicio 32\Almacenes.txt'
      filetype                      = 'ASC'
    TABLES
      data_tab                      = G_ZALMACEN.
  MODIFY zalmacen50 FROM TABLE G_ZALMACEN.
  MESSAGE 'Todo OK' TYPE 'A'.
  ELSEIF Download = 'X'.
    SELECT * INTO TABLE G_ZALMACEN FROM ZALMACEN50.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
      filename                        = 'C:\tmp\ejercicio 32\TABLA_ALMACENES50.TXT'
      filetype                        = 'ASC'
       append                          = 'X'
      write_field_separator           = 'X'
     TABLES
      data_tab                        = g_zalmacen.
      MESSAGE 'Todo OK' TYPE 'A'.
ENDIF.


*&---------------------------------------------------------------------*
*&  Include           ZDTA81L1_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF t1,
      ident    TYPE zalmacen81-ident,
*      ident2   TYPE zalmacen81-ident2,
      nombre   TYPE zalmacen81-nombre,
      area     TYPE zalmacen81-area,
      tipo     TYPE zalmacen81-tipo,
      centro   TYPE zalmacen81-centro,
      alma_sap TYPE zalmacen81-alma_sap,
*      BNAME    TYPE ZALMACEN81-BENAME,
*      FEMOD    TYPE ZALMACEN81-FEMOD, " se cogen  del sistema
      END OF t1.
DATA:
      t_t1 TYPE t1,
      t_zalmacen81 TYPE zalmacen81,

      g_t1 TYPE STANDARD TABLE OF t1,
      g_zalmacen81 TYPE STANDARD TABLE OF zalmacen81.
	  
	  
	  
*&---------------------------------------------------------------------*
*& Report  ZDTA50L1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZDTA50L1.
*PARAMETERS : Upload RADIOBUTTON GROUP RB1,
*Download RADIOBUTTON GROUP RB1.
DATA: G_ZALMACEN TYPE TABLE OF ZALMACEN50 WITH HEADER LINE,
      GT_ZALMACEN TYPE TABLE OF ZALMACEN50 WITH HEADER LINE.
*IF Upload = 'X'.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = 'C:\tmp\ejercicio 32\Almacenes.txt'
      filetype                      = 'ASC'
    TABLES
      data_tab                      = G_ZALMACEN.
*  LOOP AT G_ZALMACEN INTO GT_ZALMACEN.
*    GT_ZALMACEN-IDENT = G_ZALMACEN-IDENT.
*    GT_ZALMACEN-NOMBRE = G_ZALMACEN-NOMBRE.
*    GT_ZALMACEN-AREA = G_ZALMACEN-AREA.
*    GT_ZALMACEN-TIPO = G_ZALMACEN-TIPO.
*    GT_ZALMACEN-CENTRO = G_ZALMACEN-CENTRO.
*    GT_ZALMACEN-ALMA_SAP = G_ZALMACEN-ALMA_SAP.
*    GT_ZALMACEN-BNAME = sy-uname.
*    GT_ZALMACEN-FEMOD = sy-datum.
*  ENDLOOP.
  MODIFY zalmacen50 FROM TABLE G_ZALMACEN.
*  IF SY-SUBRC = 0.
*  MESSAGE 'Todo OK' TYPE 'A'.
*  ENDIF.
*  ELSEIF Download = 'X'.
*    SELECT * INTO TABLE G_ZALMACEN FROM ZALMACEN50.
*    CALL FUNCTION 'GUI_DOWNLOAD'
*      EXPORTING
*      filename                        = 'C:\tmp\ejercicio 32\TABLA_ALMACENES50.TXT'
*      filetype                        = 'ASC'
*       append                          = 'X'
*      write_field_separator           = 'X'
*     TABLES
*      data_tab                        = g_zalmacen.
*      MESSAGE 'Todo OK' TYPE 'A'.
*ENDIF.


	
