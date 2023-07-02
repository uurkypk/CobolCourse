//JDAYCALC JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(DAYCALC),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(DAYCALC),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN     EXEC PGM=DAYCALC
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//DATEREC   DD DSN=&SYSUID..QSAM.BB,DISP=SHR
//PRTLINE   DD DSN=&SYSUID..QSAM.CC,DISP=(NEW,CATLG,DELETE),
//          SPACE=(CYL,(10,5),RLSE)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
