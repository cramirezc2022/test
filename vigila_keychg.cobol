%eglob5#d111>citi_net>ver_2.3>tools>vigila_keychg.cobol

        0500-PROCESA-LOG.

              PERFORM 0600-READ-FILE THRU 0600-EXIT.

              IF ERROR-CODE EQUAL E$END_OF_FILE THEN
                 CALL 'S$SLEEP' USING !COMP-5(1024), ERROR-CODE
                 CALL 'S$STD_DATE_TIME' USING TMP-DATE-STR
                 MOVE TMP-DATE-STR(1 : 8) TO AA-MM-DD
                 IF AA-MM-DD NOT EQUAL AA-MM-DD-ACTUAL THEN
                    PERFORM 0200-PROC-FILE-NAMES THRU 0200-EXIT
                 END-IF
                 IF flag-cambiar = 1 THEN
                    PERFORM 0370-DETACH-PUERTOS THRU 0370-EXIT
                    PERFORM 0300-OPEN-FILE      THRU 0300-EXIT
                 END-IF
                 GOTO 0500-EXIT
              END-IF.

              IF ERROR-CODE equal 0 THEN
			                END-IF.

              PERFORM 0800-CALIFICA  THRU 0800-EXIT.

        0500-EXIT.
            EXIT.
			
        0800-CALIFICA.
      *
      * Valido que el intercambio de llaves haya sido exitoso
      * Leyando el log del ambiente y buscando 
	  * el mensaje 'key chg req from MCI successful'.
      
			 MOVE 0 TO FLAG-CALIFICA.
             MOVE !index( IN-REC-BUF(1 : IN-REC-LEN),
                          'key chg req from MCI successful')
                  TO I.
             IF I < 1 THEN GOTO 0800-EXIT.
      *
      * Valida hora de registro debe ser menor a 2 segundos de diferencia
      * respecto a hora actual.
      *
             move IN-REC-BUF(35: 4) to HORA-LOG-X.
             call 's$int_date_time' using HORA-MAQUINA.
             if HORA-MAQUINA - HORA-LOG > 2 then
                display value of 'Registro ', CONT-REC-LEI, ' Hora: ',
                                 HORA-LOG, ' muy viejo, son las: ',
                                 HORA-MAQUINA
                GOTO 0800-EXIT
             end-if.
      *			
      * Si aplica, entonces copiamos el archivo.
      *
             PERFORM 0900-COPIA-ARCHIVO
                THRU 0900-EXIT.

		0800-EXIT.
             EXIT.

        0900-COPIA-ARCHIVO.
      *			
      * La copia se hace invocando la macro copia_kb.cm
      *

			display value of !date(), '_', !time(), ' - ',
                              CONT-REC-LEI, ' Deteniendo proceso : ',
                              PROCESO.

            move '.' to CURRENT-DIR-PATH.
            call 's$expand_path' using
               CURRENT-DIR-PATH, NULL,CURRENT-DIR-PATH, ERROR-CODE.

            move !concat('copia_kb.cm',
                        PROCESO) to SP-COMMAND-LINE.

            move CURRENT-DIR-PATH       to SP-CURRENT-DIR-NAME.
            move -1                     to SP-PROCESS-PRIORITY.
            move ''                     to SP-TERMINATION-EVENT.
            move ''                     to SP-MODULE-NAME.
            call 's$expand_module_name' using SP-MODULE-NAME,				
                                             SP-MODULE-NAME,
                                             ERROR-CODE
            end-call.
            move 1                     to SP-PRIVILEGED-SWITCH.
            move !date() to COMMAND-DATE.
            move !time() to COMMAND-TIME.
            move !concat('Keychg_',PROCESO,COMMAND-DATE-TIME-X)
              to SP-PROCESS-NAME.

            move !concat(CURRENT-DIR-PATH,'>logs>',SP-PROCESS-NAME,
                        '.out') to SP-OUTPUT-PATH.

            call 's$start_process' using
                                  SP-COMMAND-LINE,
                                  SP-OUTPUT-PATH,
                                  SP-CURRENT-DIR-NAME,
                                  SP-PROCESS-PRIORITY,
                                  SP-TERMINATION-EVENT,
                                  SP-MODULE-NAME,			
                                  SP-PRIVILEGED-SWITCH,
                                  SP-PROCESS-NAME,
                                  SP-PROCESS-ID,
                                  ERROR-CODE
            end-call.
            if ERROR-CODE not equal 0 then
              display 'Problemas al levantar proceso para copiar!!'
              goto 0900-EXIT
            else
              display value of !date(), '_', !time(), ' - ',
                                SP-PROCESS-ID,
                            ' ',SP-PROCESS-NAME, 'Archivo Copiado!'
              move SP-PROCESS-ID to PI-PROCESS-ID
              call 's$sleep' using !comp-5(5120), ERROR-CODE
            end-if.


		0900-WAIT.
           display value of !date(), '_', !time(), ' - ',			
                            SP-PROCESS-ID,
                   ' ', SP-PROCESS-NAME, ' Validando FIN Proceso'.

           move 4 to PI-VERSION
           call 's$get_process_info' using PI-PROCESS-ID,
                                           PI-PROCESS-INFO,
                                           PROCESS-ERROR-CODE
           end-call
           if PROCESS-ERROR-CODE equal 0  and
              PI-PROCESS-NAME equal SP-PROCESS-NAME and
              PI-PROCESS-STATE not equal 0
           then
               display value of !date(), '_', !time(), ' - ',
                            SP-PROCESS-ID,
                        ' ', SP-PROCESS-NAME, ' Proceso Activo'
               call 's$sleep' using !comp-5(5120), ERROR-CODE
               goto 0900-WAIT
           end-if.

           display value of !date(), '_', !time(), ' - ',		   
                            SP-PROCESS-ID,
                        ' ', SP-PROCESS-NAME, ' Proceso ya NO existe'.

       0900-EXIT.
            EXIT.

       1000-VALIDA-ERROR.

           IF (ERROR-CODE NOT EQUAL TO 0) AND
              (ERROR-CODE NOT EQUAL TO E$END_OF_FILE)
           THEN
            THEN
                DISPLAY VALUE OF 'ERROR DETECTADO:', ERROR-CODE
                CALL 'SF$ERROR' USING ERROR-CODE,
                                   !display-2('vigila_keychg'),
                                   ERROR-MSG
                END-CALL
                perform 0370-DETACH-PUERTOS THRU 0370-EXIT
                GO TO 0000-EXIT-PROGRAM.

        1000-EXIT.
            EXIT.		   
		   
		   
		   
		   
		   