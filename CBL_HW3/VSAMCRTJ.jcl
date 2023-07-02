//JVSAM001 JOB ' ',CLASS=A,MSGLEVEL=(1,1),
//         MSGCLASS=X,NOTIFY=&SYSUID
//DELET500 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE Z95633.VSAM.AA CLUSTER PURGE
  IF LASTCC LE 08 THEN SET MAXCC = 00
        DEF CL ( NAME(Z95633.VSAM.AA)       -
                 FREESPACE( 20 20 )         -
                 SHR( 2,3 )                 -
                 KEYS(5 0)                  -
                 INDEXED SPEED              -
                 RECSZ(47 47)               -
                 TRK (10 10)                -
                 LOG(NONE)                  -
                 VOLUMES (VPWRKB)           -
                 UNIQUE )                   -
        DATA ( NAME(Z95633.VSAM.AA.DATA))   -
        INDEX ( NAME(Z95633.VSAM.AA.INDEX))
//REPRO600 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INN001 DD DSN=Z95633.QSAM.BB,DISP=SHR
//OUT001 DD DSN=Z95633.VSAM.AA,DISP=SHR
//SYSIN DD *
   REPRO INFILE(INN001) OUTFILE(OUT001)
