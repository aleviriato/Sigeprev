CREATE OR REPLACE PACKAGE USER_IPESP.PAC_EVENTOS_RATEIOS_NPM IS
  -- Atlantic (17/08/2020)

  V_NOME_PROC VARCHAR2(50) := 'PAC_EVENTOS_RATEIOS_NPM';

  -- Origem das Chamadas Externas
  V_FLUXO                  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE:= 1;
  V_TELA                   TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE:= 2;
  V_SEADE                  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE:= 3;
  V_SISOBI                 TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE:= 4;
  V_EXTINCAO_MAIOR_IDADE   TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE:= 5;

  TYPE t_Reg_RATEIO IS RECORD
      (NUM_SEQ             VARCHAR2(1000)
      ,COD_INS             VARCHAR2(1000)
      ,COD_BENEFICIO       VARCHAR2(1000)
      ,COD_IDE_CLI_BEN     VARCHAR2(1000)
      ,SEQ_PERIODO         VARCHAR2(1000)
      ,NUM_SEQ_RAT         VARCHAR2(1000)
      ,DAT_INI_VIG         VARCHAR2(1000)
      ,DAT_FIM_VIG         VARCHAR2(1000)
      ,VAL_PERCENT_RATEIO  VARCHAR2(1000)
      ,COD_ORIGEM          VARCHAR2(1000)
      ,FLG_STATUS          VARCHAR2(1000)
      ,USU_PROC            VARCHAR2(1000)
      );
  TYPE t_Tab_RATEIOS IS TABLE OF t_Reg_RATEIO INDEX BY BINARY_INTEGER;

  TYPE t_Reg_PERIODO IS RECORD
       (SEQ_PERIODO            TB_BENEFICIO_RATEIO_PENSAO.SEQ_PERIODO%TYPE
       ,DAT_INI_VIG            TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE
       ,DAT_FIM_VIG            TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
       ,VAL_TOT_PERCENT_RATEIO TB_BENEFICIO_RATEIO_PENSAO.VAL_PERCENT_RATEIO%TYPE
       ,FLG_ACAO               CHAR(1)
       );
  TYPE t_Tab_PERIODOS IS TABLE OF t_Reg_PERIODO INDEX BY BINARY_INTEGER;

  TYPE t_Reg_BENEF_PER IS RECORD
       (SEQ_PERIODO         TB_BENEFICIO_RATEIO_PENSAO.SEQ_PERIODO%TYPE
       ,COD_INS             TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
       ,COD_BENEFICIO       TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
       ,COD_IDE_CLI_BEN     TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
       ,DAT_INI_VIG         TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE
       ,DAT_FIM_VIG         TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
       ,VAL_PERCENT_RATEIO  TB_BENEFICIO_RATEIO_PENSAO.VAL_PERCENT_RATEIO%TYPE
       ,COD_ORIGEM          TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
       ,USU_PROC            TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
       ,FLG_STATUS          VARCHAR2(1000)
       ,MSG_ERRO            CLOB
       );
  TYPE t_Tab_BENEF_PER IS TABLE OF t_Reg_BENEF_PER INDEX BY BINARY_INTEGER;

  PROCEDURE SP_EVENTOS_RATEIOS
  (
     P_QTDE_LINHAS IN  NUMBER
    ,P_Tab_RATEIOS IN  CLOB
    ,P_COD_ERRO    OUT NUMBER
    ,P_MSG_ERRO    OUT VARCHAR2
  );

  PROCEDURE SP_EVENTOS_RATEIOS_EXTINCAO
  (
     P_COD_INS         IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
    ,P_COD_BENEFICIO   IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
    ,P_COD_IDE_CLI_BEN IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
    ,P_DAT_EXTINCAO    IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
    ,P_COD_ORIGEM      IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
    ,P_USU_PROC        IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
    ,P_COD_ERRO        OUT NUMBER
    ,P_MSG_ERRO        OUT VARCHAR2
  );

  PROCEDURE SP_EVENTOS_FLUXO_RATEIOS
  (
     P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
    ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
    ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
    ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
    ,P_COD_ERRO     OUT NUMBER
    ,P_MSG_ERRO     OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS
  (
     P_QTDE_LINHAS IN  NUMBER
    ,P_Tab_RATEIOS IN  CLOB
    ,P_COD_ERRO    OUT NUMBER
    ,P_MSG_ERRO    OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS_EXTINCAO
  (
     P_COD_INS         IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
    ,P_COD_BENEFICIO   IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
    ,P_COD_IDE_CLI_BEN IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
    ,P_DAT_EXTINCAO    IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
    ,P_COD_ORIGEM      IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
    ,P_USU_PROC        IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
    ,P_COD_ERRO        OUT NUMBER
    ,P_MSG_ERRO        OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL
  (
    P_QTDE_LINHAS       IN  NUMBER
   ,P_Tab_RATEIOS_Texto IN  CLOB
   ,P_Tab_RATEIOS       OUT t_Tab_RATEIOS
   ,P_COD_ERRO          OUT NUMBER
   ,P_MSG_ERRO          OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL_EXTINCAO
  (
    P_COD_INS             IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
   ,P_COD_BENEFICIO       IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN_EXT IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
   ,P_DAT_EXTINCAO        IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
   ,P_COD_ORIGEM          IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
   ,P_Tab_RATEIOS         OUT t_Tab_RATEIOS
   ,P_USU_PROC            IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO            OUT NUMBER
   ,P_MSG_ERRO            OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_APLICA
  (
    P_Tab_RATEIOS    IN  t_Tab_RATEIOS
   ,P_COMMIT_EXTERNO IN  BOOLEAN
   ,P_COD_ERRO       OUT NUMBER
   ,P_MSG_ERRO       OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_PERIODOS
  (
    P_Tab_RATEIOS  IN OUT  t_Tab_RATEIOS
   ,P_Tab_PERIODOS OUT t_Tab_PERIODOS
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_BENEFICIARIOS_PERIODOS
  (
    P_Tab_RATEIOS   IN  t_Tab_RATEIOS
   ,P_Tab_BENEF_PER OUT t_Tab_BENEF_PER
   ,P_COD_ERRO      OUT NUMBER
   ,P_MSG_ERRO      OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERIODOS
  (
    P_Tab_PERIODOS  IN     t_Tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERCENTUAL_RATEIO
  (
    P_Tab_PERIODOS  IN OUT t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_CANCELAMENTO
  (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_ALTERACAO
  (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_INCLUSAO
  (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VERIFICA_ERROS_VALIDACAO
  (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA
  (
    P_Tab_PERIODOS   IN  t_Tab_PERIODOS
   ,P_Tab_BENEF_PER  IN  t_Tab_BENEF_PER
   ,P_COMMIT_EXTERNO IN  BOOLEAN
   ,P_COD_ERRO       OUT NUMBER
   ,P_MSG_ERRO       OUT VARCHAR2
  );

  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO
  (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
  );
  
   PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_INCLUSAO
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
 );
 
 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_RESERVA
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
 );

  -- Funções
  FUNCTION FN_VALOR_BENEFICIO
  (
    P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
   ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
   ,P_VALOR            IN  NUMBER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  )
  RETURN NUMBER;

  FUNCTION FN_VALIDA_RATEIO_NOVA_LEI
  (
    P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
   ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  )
  RETURN BOOLEAN;

  FUNCTION FN_BENEFICIO_RATEIO_PENSAO_VALOR_BENEFICIO
  (
    P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
   ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
   ,P_VALOR            IN  NUMBER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  )
  RETURN NUMBER;

  FUNCTION FN_BENEFICIO_RATEIO_PENSAO_VALIDA_RATEIO_NOVA_LEI
  (
    P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
   ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
  )
  RETURN BOOLEAN;





END PAC_EVENTOS_RATEIOS_NPM;
/
CREATE OR REPLACE PACKAGE BODY USER_IPESP.PAC_EVENTOS_RATEIOS_NPM IS
  -- Atlantic (17/08/2020)

  -- ************* Procedimentos


  PROCEDURE SP_EVENTOS_RATEIOS
  (
     P_QTDE_LINHAS IN  NUMBER
    ,P_Tab_RATEIOS IN  CLOB
    ,P_COD_ERRO    OUT NUMBER
    ,P_MSG_ERRO    OUT VARCHAR2
  )
  IS

  BEGIN
     SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS(P_QTDE_LINHAS
                                               ,P_Tab_RATEIOS
                                               ,P_COD_ERRO
                                               ,P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
        RETURN;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

  END SP_EVENTOS_RATEIOS;


  PROCEDURE SP_EVENTOS_RATEIOS_EXTINCAO
  (
     P_COD_INS         IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
    ,P_COD_BENEFICIO   IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
    ,P_COD_IDE_CLI_BEN IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
    ,P_DAT_EXTINCAO    IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
    ,P_COD_ORIGEM      IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
    ,P_USU_PROC        IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
    ,P_COD_ERRO        OUT NUMBER
    ,P_MSG_ERRO        OUT VARCHAR2
  )
  IS

  BEGIN
      SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS_EXTINCAO(P_COD_INS
                                                         ,P_COD_BENEFICIO
                                                         ,P_COD_IDE_CLI_BEN
                                                         ,P_DAT_EXTINCAO
                                                         ,P_COD_ORIGEM
                                                         ,P_USU_PROC
                                                         ,P_COD_ERRO
                                                         ,P_MSG_ERRO);
      IF P_COD_ERRO <> 0 THEN
         RETURN;
      END IF;

      -- Procedimento Realizado com Sucesso
      P_COD_ERRO := 0;
      P_MSG_ERRO := ' ';

      EXCEPTION
          WHEN OTHERS THEN
              P_COD_ERRO := SQLCODE;
              P_MSG_ERRO := SQLERRM;

 END SP_EVENTOS_RATEIOS_EXTINCAO;


 PROCEDURE SP_EVENTOS_FLUXO_RATEIOS
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
 )
 IS

 BEGIN
    SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO(P_COD_INS
                                                  ,P_COD_ADM_TRA
                                                  ,P_COD_REGISTRO
                                                  ,P_USU_PROC
                                                  ,P_COD_ERRO
                                                  ,P_MSG_ERRO);
    IF P_COD_ERRO <> 0 THEN
        RETURN;
    END IF;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
        WHEN OTHERS THEN
            P_COD_ERRO := SQLCODE;
            P_MSG_ERRO := SQLERRM;

  END SP_EVENTOS_FLUXO_RATEIOS;


  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS
  (
     P_QTDE_LINHAS IN  NUMBER
    ,P_Tab_RATEIOS IN  CLOB
    ,P_COD_ERRO    OUT NUMBER
    ,P_MSG_ERRO    OUT VARCHAR2
  )
  IS

  V_Tab_RATEIOS  t_Tab_RATEIOS;

  BEGIN
     SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL(P_QTDE_LINHAS, P_Tab_RATEIOS, V_Tab_RATEIOS, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_APLICA(V_Tab_RATEIOS, FALSE, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS_EXTINCAO
 (
    P_COD_INS         IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
   ,P_COD_BENEFICIO   IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
   ,P_DAT_EXTINCAO    IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
   ,P_COD_ORIGEM      IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
   ,P_USU_PROC        IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO        OUT NUMBER
   ,P_MSG_ERRO        OUT VARCHAR2
 )
 IS

 V_Tab_RATEIOS  t_Tab_RATEIOS;

 BEGIN
     SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL_EXTINCAO(P_COD_INS, P_COD_BENEFICIO, P_COD_IDE_CLI_BEN, P_DAT_EXTINCAO, P_COD_ORIGEM, V_Tab_RATEIOS, P_USU_PROC, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_APLICA(V_Tab_RATEIOS, TRUE, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_RATEIOS_EXTINCAO;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL
 (
    P_QTDE_LINHAS       IN  NUMBER
   ,P_Tab_RATEIOS_Texto IN  CLOB
   ,P_Tab_RATEIOS       OUT t_Tab_RATEIOS
   ,P_COD_ERRO          OUT NUMBER
   ,P_MSG_ERRO          OUT VARCHAR2
 )
 IS

 V_Tab_RATEIOS t_Tab_RATEIOS := t_Tab_RATEIOS();

 V_CARACTER  CHAR(1);

 V_POS       NUMBER(8);
 V_LIN       NUMBER(8);
 V_COL       NUMBER(8);

 BEGIN
     IF P_QTDE_LINHAS > 0 AND INSTR(NVL(P_Tab_RATEIOS_Texto,' '),'|') > 0 THEN
         V_POS := 1;

         FOR V_LIN IN 1 .. P_QTDE_LINHAS
         LOOP
             V_Tab_RATEIOS(V_LIN).NUM_SEQ            := ' ';
             V_Tab_RATEIOS(V_LIN).COD_INS            := ' ';
             V_Tab_RATEIOS(V_LIN).COD_BENEFICIO      := ' ';
             V_Tab_RATEIOS(V_LIN).COD_IDE_CLI_BEN    := ' ';
             V_Tab_RATEIOS(V_LIN).SEQ_PERIODO        := ' ';
             V_Tab_RATEIOS(V_LIN).NUM_SEQ_RAT        := ' ';
             V_Tab_RATEIOS(V_LIN).DAT_INI_VIG        := ' ';
             V_Tab_RATEIOS(V_LIN).DAT_FIM_VIG        := ' ';
             V_Tab_RATEIOS(V_LIN).VAL_PERCENT_RATEIO := ' ';
             V_Tab_RATEIOS(V_LIN).COD_ORIGEM         := ' ';
             V_Tab_RATEIOS(V_LIN).FLG_STATUS         := ' ';
             V_Tab_RATEIOS(V_LIN).USU_PROC           := ' ';

             V_COL := 1;

             LOOP
                 V_CARACTER := SUBSTR(P_Tab_RATEIOS_Texto,V_POS,1);
                 V_POS := V_POS + 1;

                 CASE V_CARACTER
                     WHEN ';' THEN
                         V_COL := V_COL + 1;

                     WHEN '|' THEN
                         EXIT;

                     ELSE
                         CASE V_COL
                             WHEN 1 THEN
                                 V_Tab_RATEIOS(V_LIN).NUM_SEQ            := TRIM(V_Tab_RATEIOS(V_LIN).NUM_SEQ)            || V_CARACTER;
                             WHEN 2 THEN
                                 V_Tab_RATEIOS(V_LIN).COD_INS            := TRIM(V_Tab_RATEIOS(V_LIN).COD_INS)            || V_CARACTER;
                             WHEN 3 THEN
                                 V_Tab_RATEIOS(V_LIN).COD_BENEFICIO      := TRIM(V_Tab_RATEIOS(V_LIN).COD_BENEFICIO)      || V_CARACTER;
                             WHEN 4 THEN
                                 V_Tab_RATEIOS(V_LIN).COD_IDE_CLI_BEN    := TRIM(V_Tab_RATEIOS(V_LIN).COD_IDE_CLI_BEN)    || V_CARACTER;
                             WHEN 5 THEN
                                 V_Tab_RATEIOS(V_LIN).SEQ_PERIODO        := TRIM(V_Tab_RATEIOS(V_LIN).SEQ_PERIODO)        || V_CARACTER;
                             WHEN 6 THEN
                                 V_Tab_RATEIOS(V_LIN).NUM_SEQ_RAT        := TRIM(V_Tab_RATEIOS(V_LIN).NUM_SEQ_RAT)        || V_CARACTER;
                             WHEN 7 THEN
                                 V_Tab_RATEIOS(V_LIN).DAT_INI_VIG        := TRIM(V_Tab_RATEIOS(V_LIN).DAT_INI_VIG)        || V_CARACTER;
                             WHEN 8 THEN
                                 V_Tab_RATEIOS(V_LIN).DAT_FIM_VIG        := TRIM(V_Tab_RATEIOS(V_LIN).DAT_FIM_VIG)        || V_CARACTER;
                             WHEN 9 THEN
                                 V_Tab_RATEIOS(V_LIN).VAL_PERCENT_RATEIO := replace(TRIM(V_Tab_RATEIOS(V_LIN).VAL_PERCENT_RATEIO),'.',',') || V_CARACTER;
                             WHEN 10 THEN
                                 V_Tab_RATEIOS(V_LIN).COD_ORIGEM         := TRIM(V_Tab_RATEIOS(V_LIN).COD_ORIGEM)         || V_CARACTER;
                             WHEN 11 THEN
                                 V_Tab_RATEIOS(V_LIN).FLG_STATUS         := TRIM(V_Tab_RATEIOS(V_LIN).FLG_STATUS)         || V_CARACTER;
                             WHEN 12 THEN
                                 V_Tab_RATEIOS(V_LIN).USU_PROC           := TRIM(V_Tab_RATEIOS(V_LIN).USU_PROC)           || V_CARACTER;
                         END CASE;
                 END CASE;
             END LOOP;
         END LOOP;

         -- Ajusta Nulos
         FOR a IN 1 .. V_Tab_RATEIOS.COUNT
         LOOP
             IF UPPER(TRIM(V_Tab_RATEIOS(a).NUM_SEQ)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).NUM_SEQ := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).COD_INS)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).COD_INS := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).COD_BENEFICIO)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).COD_BENEFICIO := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).COD_IDE_CLI_BEN)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).COD_IDE_CLI_BEN := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).SEQ_PERIODO)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).SEQ_PERIODO := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).NUM_SEQ_RAT)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).NUM_SEQ_RAT := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).DAT_INI_VIG)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).DAT_INI_VIG := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).DAT_FIM_VIG)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).DAT_FIM_VIG := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).VAL_PERCENT_RATEIO)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).VAL_PERCENT_RATEIO := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).COD_ORIGEM)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).COD_ORIGEM := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).FLG_STATUS)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).FLG_STATUS := NULL;
             END IF;
             IF UPPER(TRIM(V_Tab_RATEIOS(a).USU_PROC)) = 'NULL' THEN
                 V_Tab_RATEIOS(a).USU_PROC := NULL;
             END IF;
         END LOOP;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     P_Tab_RATEIOS := V_Tab_RATEIOS;

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL_EXTINCAO
 (
    P_COD_INS             IN  TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
   ,P_COD_BENEFICIO       IN  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
   ,P_COD_IDE_CLI_BEN_EXT IN  TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE
   ,P_DAT_EXTINCAO        IN  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE
   ,P_COD_ORIGEM          IN  TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE
   ,P_Tab_RATEIOS         OUT t_Tab_RATEIOS
   ,P_USU_PROC            IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO            OUT NUMBER
   ,P_MSG_ERRO            OUT VARCHAR2
 )
 IS

 CURSOR C_RATEIOS
             (P_COD_INS        TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE
             ,P_COD_BENEFICIO  TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE
             )
 IS
   SELECT    NUM_SEQ
            ,COD_INS
            ,COD_BENEFICIO
            ,COD_IDE_CLI_BEN
            ,SEQ_PERIODO
            ,NUM_SEQ_RAT
            ,DAT_INI_VIG
            ,DAT_FIM_VIG
            ,VAL_PERCENT_RATEIO
            ,FLG_STATUS
   FROM      TB_BENEFICIO_RATEIO_PENSAO A
   WHERE     COD_INS         = P_COD_INS
   AND       COD_BENEFICIO   = P_COD_BENEFICIO
   AND      (DAT_FIM_VIG IS NULL -- Vigente
             OR
            (DAT_FIM_VIG >= SYSDATE AND FLG_STATUS = 'V')) -- Período Futuro Vigente
   ORDER BY  COD_INS
            ,COD_BENEFICIO
            ,SEQ_PERIODO
            ,COD_IDE_CLI_BEN;

  V_Tab_RATEIOS  t_Tab_RATEIOS;

  V_SEQ_RATEIO   TB_BENEFICIO_RATEIO_PENSAO.NUM_SEQ_RAT%TYPE;

  V_REG NUMBER(8) := 0;

  BEGIN
     -- Prepara Para Findar o Período Vigente Com a Participação do Beneficiário a Ser Extinto
     FOR C_Reg_Rateios IN C_RATEIOS(P_COD_INS, P_COD_BENEFICIO)
     LOOP
         V_REG := V_Tab_RATEIOS.COUNT + 1;
         --
         IF C_Reg_Rateios.FLG_STATUS = 'V' THEN
             V_Tab_RATEIOS(V_REG).NUM_SEQ            := C_Reg_Rateios.NUM_SEQ;
             V_Tab_RATEIOS(V_REG).COD_INS            := C_Reg_Rateios.COD_INS;
             V_Tab_RATEIOS(V_REG).COD_BENEFICIO      := C_Reg_Rateios.COD_BENEFICIO;
             V_Tab_RATEIOS(V_REG).COD_IDE_CLI_BEN    := C_Reg_Rateios.COD_IDE_CLI_BEN;
             V_Tab_RATEIOS(V_REG).SEQ_PERIODO        := C_Reg_Rateios.SEQ_PERIODO;
             V_Tab_RATEIOS(V_REG).NUM_SEQ_RAT        := C_Reg_Rateios.NUM_SEQ_RAT;
             V_Tab_RATEIOS(V_REG).DAT_INI_VIG        := C_Reg_Rateios.DAT_INI_VIG;
             V_Tab_RATEIOS(V_REG).DAT_FIM_VIG        := P_DAT_EXTINCAO; -- Finda a Vigência
             V_Tab_RATEIOS(V_REG).VAL_PERCENT_RATEIO := C_Reg_Rateios.VAL_PERCENT_RATEIO;
             V_Tab_RATEIOS(V_REG).COD_ORIGEM         := P_COD_ORIGEM; -- Origem do Processamento
             V_Tab_RATEIOS(V_REG).FLG_STATUS         := C_Reg_Rateios.FLG_STATUS;
             V_Tab_RATEIOS(V_REG).USU_PROC           := P_USU_PROC;
         END IF;
     END LOOP;

     -- Prepara Para Abrir o Novo Período Com a Participação Apenas dos Beneficiários Remanescentes
     FOR C_Reg_Rateios IN C_RATEIOS(P_COD_INS, P_COD_BENEFICIO)
     LOOP
         V_REG        := V_Tab_RATEIOS.COUNT + 1;
         V_SEQ_RATEIO := V_SEQ_RATEIO + 1;

         IF C_Reg_Rateios.FLG_STATUS = 'V' THEN
             IF C_Reg_Rateios.COD_IDE_CLI_BEN <> P_COD_IDE_CLI_BEN_EXT THEN
                 V_Tab_RATEIOS(V_REG).NUM_SEQ            := C_Reg_Rateios.NUM_SEQ;
                 V_Tab_RATEIOS(V_REG).COD_INS            := C_Reg_Rateios.COD_INS;
                 V_Tab_RATEIOS(V_REG).COD_BENEFICIO      := C_Reg_Rateios.COD_BENEFICIO;
                 V_Tab_RATEIOS(V_REG).COD_IDE_CLI_BEN    := C_Reg_Rateios.COD_IDE_CLI_BEN;
                 V_Tab_RATEIOS(V_REG).SEQ_PERIODO        := 99; -- Indica Novo Período
                 V_Tab_RATEIOS(V_REG).NUM_SEQ_RAT        := V_SEQ_RATEIO;
                 V_Tab_RATEIOS(V_REG).DAT_INI_VIG        := P_DAT_EXTINCAO + 1; -- Vigência Nova
                 V_Tab_RATEIOS(V_REG).DAT_FIM_VIG        := NULL; -- Vigência Nova Aberta
                 V_Tab_RATEIOS(V_REG).VAL_PERCENT_RATEIO := C_Reg_Rateios.VAL_PERCENT_RATEIO;
                 V_Tab_RATEIOS(V_REG).COD_ORIGEM         := P_COD_ORIGEM; -- Origem do Processamento
                 V_Tab_RATEIOS(V_REG).FLG_STATUS         := C_Reg_Rateios.FLG_STATUS;
                 V_Tab_RATEIOS(V_REG).USU_PROC           := P_USU_PROC;
             END IF;
         END IF;
     END LOOP;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     P_Tab_RATEIOS := V_Tab_RATEIOS;

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

  END SP_BENEFICIO_RATEIO_PENSAO_PREPARA_MATRIZ_LOCAL_EXTINCAO;


  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_APLICA
  (
     P_Tab_RATEIOS    IN  t_Tab_RATEIOS
    ,P_COMMIT_EXTERNO IN  BOOLEAN
    ,P_COD_ERRO       OUT NUMBER
    ,P_MSG_ERRO       OUT VARCHAR2
  )
  IS

  V_Tab_RATEIOS   t_Tab_RATEIOS;
  V_Tab_PERIODOS  t_Tab_PERIODOS;
  V_Tab_BENEF_PER t_Tab_BENEF_PER;

  BEGIN
     V_Tab_RATEIOS := P_Tab_RATEIOS;

     SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_PERIODOS(V_Tab_RATEIOS, V_Tab_PERIODOS, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_BENEFICIARIOS_PERIODOS(V_Tab_RATEIOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERIODOS(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERCENTUAL_RATEIO(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VALIDA_CANCELAMENTO(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VALIDA_ALTERACAO(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VALIDA_INCLUSAO(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_VERIFICA_ERROS_VALIDACAO(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     SP_BENEFICIO_RATEIO_PENSAO_GRAVA(V_Tab_PERIODOS, V_Tab_BENEF_PER, P_COMMIT_EXTERNO, P_COD_ERRO, P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
         RETURN;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_EVENTOS_APLICA;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_PERIODOS
 (
    P_Tab_RATEIOS  IN OUT  t_Tab_RATEIOS
   ,P_Tab_PERIODOS OUT     t_Tab_PERIODOS
   ,P_COD_ERRO     OUT     NUMBER
   ,P_MSG_ERRO     OUT     VARCHAR2
 )
 IS

 V_EXISTE_REG   NUMBER;
 V_EXISTE       BOOLEAN;
 c              NUMBER(8);
 V_SEQ_PERIODO  TB_BENEFICIO_RATEIO_PENSAO.SEQ_PERIODO%type;

 V_Tab_PERIODOS t_Tab_PERIODOS;


 BEGIN
    FOR a IN 1 .. P_Tab_RATEIOS.COUNT
    LOOP
        V_EXISTE := FALSE;

        FOR b IN 1 .. V_Tab_PERIODOS.COUNT
        LOOP
            IF V_Tab_PERIODOS(b).SEQ_PERIODO = P_Tab_RATEIOS(a).SEQ_PERIODO THEN
                V_EXISTE := TRUE;
                EXIT;
            END IF;
        END LOOP;

        IF NOT V_EXISTE THEN
            c := V_Tab_PERIODOS.COUNT + 1;

            V_Tab_PERIODOS(c).SEQ_PERIODO            := P_Tab_RATEIOS(a).SEQ_PERIODO;
            V_Tab_PERIODOS(c).DAT_INI_VIG            := P_Tab_RATEIOS(a).DAT_INI_VIG;
            V_Tab_PERIODOS(c).DAT_FIM_VIG            := P_Tab_RATEIOS(a).DAT_FIM_VIG;
            V_Tab_PERIODOS(c).VAL_TOT_PERCENT_RATEIO := 0;

            IF P_Tab_RATEIOS(a).FLG_STATUS = 'C' THEN
                V_Tab_PERIODOS(c).FLG_ACAO := P_Tab_RATEIOS(a).FLG_STATUS; -- Cancelamento.
            ELSE
                -- Identifica Qual Operação Está Sendo Solicitada Para o Período, Através do Perfil do Registro na Matriz x Banco de Dados (Rateio Atual do Benefício).
                BEGIN
                    SELECT 1
                    INTO   V_EXISTE_REG
                    FROM   TB_BENEFICIO_RATEIO_PENSAO
                    WHERE  COD_INS       = P_Tab_RATEIOS(a).COD_INS
                    AND    COD_BENEFICIO = P_Tab_RATEIOS(a).COD_BENEFICIO
                    AND    SEQ_PERIODO   = P_Tab_RATEIOS(a).SEQ_PERIODO
                    AND    ROWNUM        = 1;
                EXCEPTION
                     WHEN OTHERS THEN
                         V_EXISTE_REG := 0;
                END;

                IF V_EXISTE_REG = 1 THEN
                    V_Tab_PERIODOS(c).FLG_ACAO := 'A'; -- Encontrou o Registro em Nível de Período, Assume que é uma Alteração.
                ELSE
                    V_Tab_PERIODOS(c).FLG_ACAO := 'I'; -- Não Encontrou o Registro em Nível de Período, Assume que é uma Inclusão.
                END IF;


            END IF;
        END IF;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    P_Tab_PERIODOS := V_Tab_PERIODOS;

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_PERIODOS;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_BENEFICIARIOS_PERIODOS
 (
    P_Tab_RATEIOS   IN  t_Tab_RATEIOS
   ,P_Tab_BENEF_PER OUT t_Tab_BENEF_PER
   ,P_COD_ERRO      OUT NUMBER
   ,P_MSG_ERRO      OUT VARCHAR2
 )
 IS

 V_EXISTE BOOLEAN;
 c        NUMBER(8);

 V_Tab_BENEF_PER t_Tab_BENEF_PER;

 BEGIN
    FOR a IN 1 .. P_Tab_RATEIOS.COUNT
    LOOP
        V_EXISTE := FALSE;

        FOR b IN 1 .. V_Tab_BENEF_PER.COUNT
        LOOP
            IF V_Tab_BENEF_PER(b).SEQ_PERIODO     = P_Tab_RATEIOS(a).SEQ_PERIODO AND
               V_Tab_BENEF_PER(b).COD_IDE_CLI_BEN = P_Tab_RATEIOS(a).COD_IDE_CLI_BEN THEN
                V_EXISTE := TRUE;
                EXIT;
            END IF;
        END LOOP;

        IF NOT V_EXISTE THEN
            c := V_Tab_BENEF_PER.COUNT + 1;

            V_Tab_BENEF_PER(c).SEQ_PERIODO        := P_Tab_RATEIOS(a).SEQ_PERIODO;
            V_Tab_BENEF_PER(c).COD_INS            := P_Tab_RATEIOS(a).COD_INS;
            V_Tab_BENEF_PER(c).COD_BENEFICIO      := P_Tab_RATEIOS(a).COD_BENEFICIO;
            V_Tab_BENEF_PER(c).COD_IDE_CLI_BEN    := P_Tab_RATEIOS(a).COD_IDE_CLI_BEN;
            V_Tab_BENEF_PER(c).DAT_INI_VIG        := P_Tab_RATEIOS(a).DAT_INI_VIG;
            V_Tab_BENEF_PER(c).DAT_FIM_VIG        := P_Tab_RATEIOS(a).DAT_FIM_VIG;
            V_Tab_BENEF_PER(c).VAL_PERCENT_RATEIO := P_Tab_RATEIOS(a).VAL_PERCENT_RATEIO;
            V_Tab_BENEF_PER(c).COD_ORIGEM         := P_Tab_RATEIOS(a).COD_ORIGEM;
            V_Tab_BENEF_PER(c).USU_PROC           := P_Tab_RATEIOS(a).USU_PROC;
            V_Tab_BENEF_PER(c).FLG_STATUS         := P_Tab_RATEIOS(a).FLG_STATUS;
        END IF;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    P_Tab_BENEF_PER := V_Tab_BENEF_PER ;

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_IDENTIFICA_BENEFICIARIOS_PERIODOS;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERIODOS
 (
    P_Tab_PERIODOS  IN     t_Tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 V_EXISTE_REG               NUMBER(1);
 V_DAT_INI_BEN              TB_BENEFICIARIO.DAT_INI_BEN%TYPE;
 V_DAT_FIM_BEN              TB_BENEFICIARIO.DAT_FIM_BEN%TYPE;
 V_FLG_STATUS               TB_BENEFICIARIO.FLG_STATUS%TYPE;

 V_PRIMEIRO                 BOOLEAN;
 V_DES_ERRO_PERIODO         VARCHAR2(2000);
 V_FLG_STATUS_PERIODO       TB_BENEFICIO_RATEIO_PENSAO.FLG_STATUS%TYPE;
 V_DAT_INI_VIG_PERIODO      TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE;
 V_DAT_FIM_VIG_PERIODO      TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE;

 V_DAT_FIM_VIG_PERIODO_ANT  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE;

 V_DES_OPERACAO             VARCHAR2(20) := 'PERÍODO';
 V_DES_ERRO                 VARCHAR2(200);

 V_NUM_SEQ                  NUMBER;

 V_DES_STATUS_BEN           VARCHAR2(20);

 BEGIN
    V_PRIMEIRO := TRUE;

    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        V_DES_ERRO_PERIODO := ' ';

        V_DAT_INI_VIG_PERIODO := P_Tab_PERIODOS(a).DAT_INI_VIG;
        V_DAT_FIM_VIG_PERIODO := P_Tab_PERIODOS(a).DAT_FIM_VIG;

        V_FLG_STATUS_PERIODO := CASE P_Tab_PERIODOS(a).FLG_ACAO
                                    WHEN 'I' THEN 'V'
                                    WHEN 'A' THEN 'V'
                                    WHEN 'C' THEN 'C'
                                    ELSE ' '
                                END;


        FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
        LOOP
            case when P_Tab_BENEF_PER(b).FLG_STATUS = 'V' then
                    V_DES_STATUS_BEN := 'VÁLIDO';
                 when P_Tab_BENEF_PER(b).FLG_STATUS = 'C' then
                    V_DES_STATUS_BEN := 'CANCELADO';
            end case;
            --
            IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                -- Validações apenas para inserção
                IF P_Tab_PERIODOS(a).FLG_ACAO = 'I' THEN
                  -- Verifica Concomitância ou Lacunas nos Períodos Vigentes.
                  SELECT COUNT(1)
                    INTO V_EXISTE_REG
                    FROM TB_BENEFICIO_RATEIO_PENSAO BRP
                   WHERE BRP.COD_INS = P_Tab_BENEF_PER(b).COD_INS
                     AND brp.COD_BENEFICIO = P_Tab_BENEF_PER(b).COD_BENEFICIO
                     AND BRP.COD_IDE_CLI_BEN =  P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                     AND V_DAT_INI_VIG_PERIODO <= NVL(BRP.DAT_FIM_VIG,V_DAT_INI_VIG_PERIODO)
                     AND NVL(V_DAT_FIM_VIG_PERIODO,BRP.DAT_INI_VIG)>= BRP.DAT_INI_VIG
                     AND BRP.FLG_STATUS = 'V';


                  IF V_EXISTE_REG > 0 THEN
                    V_DES_ERRO := '(Não é permitido inserir registro em período concomitante para beneficiário.)';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Status Informado: '     || V_DES_STATUS_BEN                       || ' | ';
                  END IF;
                END IF;
                -- Concomitância ou Lacunas Identificados nos Períodos Vigentes.
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_ERRO_PERIODO;

                -- Verifica se Todos os Beneficiários Foram Informados Com Mesma Indicação Para Manutenção do Rateio.
                IF P_Tab_BENEF_PER(b).FLG_STATUS <> V_FLG_STATUS_PERIODO THEN
                    V_DES_ERRO := '(Todos os beneficiários do período devem ter a mesma indicação para manutenção do rateio (Status). Permitido C ou V.)';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                    P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Status Informado: '     || V_DES_STATUS_BEN                       || ' | ';
                END IF;

                -- Validações Para Períodos de Rateios Vigentes
                IF V_FLG_STATUS_PERIODO <> 'C' THEN
                    -- Verifica o Benefício.
                    BEGIN
                        SELECT 1
                              ,DAT_INI_BEN
                              ,DAT_FIM_BEN
                              ,FLG_STATUS
                        INTO   V_EXISTE_REG
                              ,V_DAT_INI_BEN
                              ,V_DAT_FIM_BEN
                              ,V_FLG_STATUS
                        FROM   TB_BENEFICIARIO
                        WHERE  COD_INS         = P_Tab_BENEF_PER(b).COD_INS
                        AND    COD_BENEFICIO   = P_Tab_BENEF_PER(b).COD_BENEFICIO
                        AND    COD_IDE_CLI_BEN = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN;
                    EXCEPTION
                         WHEN OTHERS THEN
                             V_EXISTE_REG  := 0;
                             V_DAT_INI_BEN := NULL;
                             V_DAT_FIM_BEN := NULL;
                             V_FLG_STATUS  := NULL;
                    END;

                    IF V_EXISTE_REG = 0 THEN
                        V_DES_ERRO := '(O Benefício não foi encontrado).';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' | ';
                    ELSE
                        IF V_DAT_INI_BEN IS NULL THEN
                            V_DES_ERRO := '(O Benefício não possui data de início).';
                            P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                            P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                            P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                            P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' | ';
                        END IF;

                        IF P_Tab_BENEF_PER(b).COD_ORIGEM = V_TELA THEN
                            IF V_DAT_FIM_BEN < TRUNC(SYSDATE) THEN
                                V_DES_ERRO := '(O Benefício já foi finalizado).';
                                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: '    || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '               || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '            || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' | ';
                                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Término do Benefício: ' || V_DAT_FIM_BEN                      || ' | ';
                            END IF;
                        END IF;
                    END IF;

                    -- Verifica se Todos os Beneficiários Foram Informados Com o Mesmo Período de Vigência Para Manutenção do Rateio.
                    IF (P_Tab_BENEF_PER(b).DAT_INI_VIG <> V_DAT_INI_VIG_PERIODO) OR (P_Tab_BENEF_PER(b).DAT_FIM_VIG <> V_DAT_FIM_VIG_PERIODO) THEN
                        V_DES_ERRO := '(Todos os beneficiários do período devem ter o mesmo período de vigência do rateio.)';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Rateio: '  || P_Tab_BENEF_PER(b).DAT_INI_VIG     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Término do Rateio: ' || P_Tab_BENEF_PER(b).DAT_FIM_VIG     || ' | ';
                    END IF;

                    -- Verifica a Vigência do Rateio, Considerando o Ingresso no Benefício.
                    IF P_Tab_BENEF_PER(b).DAT_INI_VIG < V_DAT_INI_BEN THEN
                        V_DES_ERRO := '(O período do rateio não pode ter início de vigência anterior ao início do benefício.)';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: '   || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '              || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '           || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Benefício: ' || V_DAT_INI_BEN                      || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Rateio: '    || P_Tab_BENEF_PER(b).DAT_INI_VIG     || ' | ';
                    END IF;

                    IF P_Tab_BENEF_PER(b).DAT_INI_VIG >= NVL(V_DAT_FIM_BEN,P_Tab_BENEF_PER(b).DAT_INI_VIG+1) THEN
                        V_DES_ERRO := '(O período do rateio não pode ter início de vigência após fim do benefício.)';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: '   || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '              || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '           || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Benefício: ' || V_DAT_INI_BEN                      || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Fim do Benefício: '    || V_DAT_FIM_BEN                      || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Rateio: '    || P_Tab_BENEF_PER(b).DAT_INI_VIG     || ' | ';
                    END IF;

                    IF P_Tab_BENEF_PER(b).DAT_FIM_VIG IS NOT NULL AND P_Tab_BENEF_PER(b).DAT_FIM_VIG  > NVL(V_DAT_FIM_BEN,P_Tab_BENEF_PER(b).DAT_FIM_VIG+1) THEN
                        V_DES_ERRO := '(O período do rateio não pode ter fim de vigência após fim do benefício.)';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: '   || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '              || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '           || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Benefício: ' || V_DAT_INI_BEN                      || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Fim do Benefício: '    || V_DAT_FIM_BEN                      || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Rateio: '    || P_Tab_BENEF_PER(b).DAT_INI_VIG     || ' | ';
                    END IF;

                    -- Verifica o Intervalo da Vigência do Rateio.
                    IF P_Tab_BENEF_PER(b).DAT_INI_VIG > P_Tab_BENEF_PER(b).DAT_FIM_VIG THEN
                        V_DES_ERRO := '(A data de início da vigência do período do rateio não pode ser maior que a data de término da vigência do período do rateio.)';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: '   || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '              || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '           || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Início do Rateio: '    || P_Tab_BENEF_PER(b).DAT_INI_VIG     || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Dt.Término do Rateio: '   || P_Tab_BENEF_PER(b).DAT_FIM_VIG     || ' | ';
                    END IF;
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERIODOS;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERCENTUAL_RATEIO
 (
    P_Tab_PERIODOS  IN OUT t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 V_EXISTE_REG    NUMBER(1);
 V_COD_PERFIL    TB_BENEFICIARIO.COD_PERFIL%TYPE;
 V_DES_OPERACAO  VARCHAR2(20) := '% RATEIO';
 V_DES_ERRO      VARCHAR2(200);
 V_PER_EXCEDENTE NUMBER;

 BEGIN
    -- Verifica Percentual Total Rateado por Período.
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
        LOOP
            IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                -- Valida Para Períodos de Rateios Vigentes
                IF P_Tab_BENEF_PER(b).FLG_STATUS <> 'C' THEN
                    P_Tab_PERIODOS(a).VAL_TOT_PERCENT_RATEIO := P_Tab_PERIODOS(a).VAL_TOT_PERCENT_RATEIO + P_Tab_BENEF_PER(b).VAL_PERCENT_RATEIO;
                END IF;
            END IF;
            --
            IF P_Tab_PERIODOS(a).VAL_TOT_PERCENT_RATEIO > 100 THEN
              V_DES_ERRO := '(O valor total rateado entre os beneficiários desse período é maior que 100%).';
              P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
              P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO           || ' / ';
              P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO         || ' / ';
              P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || '% Total Rateado: '      || P_Tab_PERIODOS(a).VAL_TOT_PERCENT_RATEIO || ' | ';
              EXIT;
            END IF;
        END LOOP;
    END LOOP;
    --
    -- Identifica Inconsistência no Percentual Total Rateado.
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
        LOOP
            IF P_Tab_PERIODOS(a).FLG_ACAO = 'I' THEN
              -- Para periodo de inserção, valida se algum outro beneficiario soma mais de 100% de rati
              V_PER_EXCEDENTE := NULL;
              BEGIN
                SELECT A.PER + P_Tab_BENEF_PER(b).VAL_PERCENT_RATEIO
                  INTO V_PER_EXCEDENTE
                  FROM (SELECT COD_IDE_CLI_BEN,
                               MAX(VAL_PERCENT_RATEIO) PER
                          FROM TB_BENEFICIO_RATEIO_PENSAO  BRP
                         WHERE BRP.COD_BENEFICIO = P_Tab_BENEF_PER(b).COD_BENEFICIO
                           AND P_Tab_PERIODOS(a).DAT_INI_VIG <= BRP.DAT_FIM_VIG
                           AND nvl(P_Tab_PERIODOS(a).DAT_FIM_VIG,BRP.DAT_INI_VIG) >= BRP.DAT_INI_VIG
                           AND COD_IDE_CLI_BEN <> P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                           AND BRP.FLG_STATUS = 'V'
                      GROUP BY COD_IDE_CLI_BEN) A
                        WHERE A.PER + P_Tab_BENEF_PER(b).VAL_PERCENT_RATEIO > 100;
              EXCEPTION
               WHEN NO_DATA_FOUND THEN
                 V_PER_EXCEDENTE:= NULL;
              END;

              IF V_PER_EXCEDENTE IS NOT NULL THEN
                V_DES_ERRO := '(O valor total de rateio para o período é maior do que 100% para um período).';
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO           || ' / ';
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO         || ' / ';
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefíciario: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN       || ' / ';
                P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || '% Excedente: '          || TRUNC(V_PER_EXCEDENTE,4)                 || ' | ';
              END IF;
            END IF;
        END LOOP;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VALIDA_PERCENTUAL_RATEIO;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_CANCELAMENTO
 (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 V_EXISTE_REG   NUMBER(1);
 V_DES_OPERACAO VARCHAR2(20) := 'CANCELAMENTO';
 V_DES_ERRO     VARCHAR2(200);

 BEGIN
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        IF P_Tab_PERIODOS(a).FLG_ACAO = 'C' THEN
            FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
            LOOP
                IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                    -- Verifica o Rateio Atual do Beneficiário.
                    BEGIN
                        SELECT 1
                        INTO   V_EXISTE_REG
                        FROM   TB_BENEFICIO_RATEIO_PENSAO
                        WHERE  COD_INS         = P_Tab_BENEF_PER(b).COD_INS
                        AND    COD_BENEFICIO   = P_Tab_BENEF_PER(b).COD_BENEFICIO
                        AND    COD_IDE_CLI_BEN = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                        AND    SEQ_PERIODO     = P_Tab_BENEF_PER(b).SEQ_PERIODO;
                    EXCEPTION
                         WHEN OTHERS THEN
                             V_EXISTE_REG := 0;
                    END;

                    IF V_EXISTE_REG <> 1 THEN
                        V_DES_ERRO := '(Beneficiário não encontrado no rateio deste período).';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' | ';
                    END IF;
                END IF;
            END LOOP;
        END IF;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VALIDA_CANCELAMENTO;


  PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_ALTERACAO
 (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 V_EXISTE_REG   NUMBER;
 V_DES_OPERACAO VARCHAR2(20) := 'ALTERAÇÃO';
 V_DES_ERRO     VARCHAR2(200);

 BEGIN
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        IF P_Tab_PERIODOS(a).FLG_ACAO = 'A' THEN
            FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
            LOOP
                IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                    -- Verifica o Rateio Atual do Beneficiário.
                    BEGIN
                        SELECT 1
                        INTO   V_EXISTE_REG
                        FROM   TB_BENEFICIO_RATEIO_PENSAO
                        WHERE  COD_INS         = P_Tab_BENEF_PER(b).COD_INS
                        AND    COD_BENEFICIO   = P_Tab_BENEF_PER(b).COD_BENEFICIO
                        AND    COD_IDE_CLI_BEN = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                        AND    SEQ_PERIODO     = P_Tab_BENEF_PER(b).SEQ_PERIODO;
                    EXCEPTION
                         WHEN OTHERS THEN
                             V_EXISTE_REG := 0;
                    END;

                    IF V_EXISTE_REG <> 1 THEN
                        V_DES_ERRO := '(Beneficiário não encontrado no rateio deste período).';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' | ';
                    END IF;
                END IF;
            END LOOP;
        END IF;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VALIDA_ALTERACAO;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VALIDA_INCLUSAO
 (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 V_EXISTE_REG   NUMBER;
 V_DES_OPERACAO VARCHAR2(20) := 'INCLUSÃO';
 V_DES_ERRO     VARCHAR2(200);

 BEGIN
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        IF P_Tab_PERIODOS(a).FLG_ACAO = 'I' THEN
            FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
            LOOP
                IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                    -- Verifica o Rateio Atual do Beneficiário.
                    BEGIN
                        SELECT 1
                        INTO   V_EXISTE_REG
                        FROM   TB_BENEFICIO_RATEIO_PENSAO
                        WHERE  COD_INS         = P_Tab_BENEF_PER(b).COD_INS
                        AND    COD_BENEFICIO   = P_Tab_BENEF_PER(b).COD_BENEFICIO
                        AND    COD_IDE_CLI_BEN = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                        AND    SEQ_PERIODO     = P_Tab_BENEF_PER(b).SEQ_PERIODO;
                    EXCEPTION
                         WHEN OTHERS THEN
                             V_EXISTE_REG := 0;
                    END;

                    IF V_EXISTE_REG = 1 THEN
                        V_DES_ERRO := '(Beneficiário já encontrado no rateio deste período).';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || V_DES_OPERACAO || ' ' || V_DES_ERRO || ' - ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Benefício: '            || P_Tab_BENEF_PER(b).COD_BENEFICIO   || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Beneficiário: '         || P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN || ' / ';
                        P_Tab_BENEF_PER(b).MSG_ERRO := P_Tab_BENEF_PER(b).MSG_ERRO || 'Sequência do Período: ' || P_Tab_BENEF_PER(b).SEQ_PERIODO     || ' | ';
                    END IF;
                END IF;
            END LOOP;
        END IF;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VALIDA_INCLUSAO;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_VERIFICA_ERROS_VALIDACAO
 (
    P_Tab_PERIODOS  IN     t_tab_PERIODOS
   ,P_Tab_BENEF_PER IN OUT t_Tab_BENEF_PER
   ,P_COD_ERRO         OUT NUMBER
   ,P_MSG_ERRO         OUT VARCHAR2
 )
 IS

 BEGIN
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    -- Retorna Todos os Erros Encontrados em Todos os Períodos
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
        LOOP
            IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                IF NVL(P_Tab_BENEF_PER(b).MSG_ERRO,' ') <> ' ' THEN
                    P_MSG_ERRO := P_MSG_ERRO || P_Tab_BENEF_PER(b).MSG_ERRO;
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    IF P_MSG_ERRO <> ' ' THEN
        P_MSG_ERRO := LTRIM(RTRIM(P_MSG_ERRO));
        P_MSG_ERRO := REPLACE(P_MSG_ERRO,'Sequência do Período: 99','Período NOVO:');
        P_COD_ERRO := 1;
    END IF;

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

 END SP_BENEFICIO_RATEIO_PENSAO_VERIFICA_ERROS_VALIDACAO;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA
 (
    P_Tab_PERIODOS   IN  t_Tab_PERIODOS
   ,P_Tab_BENEF_PER  IN  t_Tab_BENEF_PER
   ,P_COMMIT_EXTERNO IN  BOOLEAN
   ,P_COD_ERRO       OUT NUMBER
   ,P_MSG_ERRO       OUT VARCHAR2
 )
 IS

 V_PRIMEIRO          BOOLEAN;
 V_NOM_ORIGEM        TB_HIST_EVENTO.NOM_ORIGEM%TYPE := 'TelaBeneficioRateio';
 V_SEQ_PERIODO       TB_BENEFICIO_RATEIO_PENSAO.SEQ_PERIODO%TYPE;
 V_PROX_NUM_SEQ_RAT  TB_BENEFICIO_RATEIO_PENSAO.NUM_SEQ_RAT%TYPE;

 V_Tab_Hist          PAC_LOG_HIST_EVENTOS.t_Tab_Hist_BENEFICIO_RATEIO_PENSAO;

 BEGIN
    FOR a IN 1 .. P_Tab_PERIODOS.COUNT
    LOOP
        V_PRIMEIRO := TRUE;

        FOR b IN 1 .. P_Tab_BENEF_PER.COUNT
        LOOP
            IF P_Tab_BENEF_PER(b).SEQ_PERIODO = P_Tab_PERIODOS(a).SEQ_PERIODO THEN
                -- Na Inclusão, Identifica o Próximo Número de Sequência do Período. Apenas no Primeiro Registro do Rateio.
                IF P_Tab_PERIODOS(a).FLG_ACAO = 'I' THEN
                    IF V_PRIMEIRO THEN
                        -- Verificando se o periodo ja possui um seq_periodo
                        BEGIN
                          SELECT BRP.SEQ_PERIODO
                            INTO  V_SEQ_PERIODO
                            FROM  TB_BENEFICIO_RATEIO_PENSAO BRP
                           WHERE  COD_INS        = P_Tab_BENEF_PER(b).COD_INS
                             AND  COD_BENEFICIO = P_Tab_BENEF_PER(b).COD_BENEFICIO
                             AND  BRP.DAT_INI_VIG = P_Tab_PERIODOS(a).DAT_INI_VIG
                             AND  NVL(BRP.DAT_FIM_VIG,trunc(sysdate)) = nvl(P_Tab_PERIODOS(a).DAT_FIM_VIG,trunc(sysdate))
                             AND  BRP.FLG_STATUS = 'V';
                        EXCEPTION
                             WHEN NO_DATA_FOUND THEN
                                 V_SEQ_PERIODO := null;
                        END;
                        -- Se não, pega o próximo
                        IF V_SEQ_PERIODO IS NULL THEN
                          BEGIN
                              SELECT NVL(MAX(SEQ_PERIODO),0) + 1
                              INTO   V_SEQ_PERIODO
                              FROM   TB_BENEFICIO_RATEIO_PENSAO
                              WHERE  COD_INS       = P_Tab_BENEF_PER(b).COD_INS
                              AND    COD_BENEFICIO = P_Tab_BENEF_PER(b).COD_BENEFICIO;
                          EXCEPTION
                               WHEN NO_DATA_FOUND THEN
                                   V_SEQ_PERIODO := 1;
                          END;
                        END IF;
                        V_PRIMEIRO := FALSE;
                    END IF;
                ELSE
                    V_SEQ_PERIODO := P_Tab_BENEF_PER(b).SEQ_PERIODO;
                END IF;

                -- Histórico - Prepara Registro Antes da Operação.
                PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_MONTA_HIST(P_Tab_BENEF_PER(b).COD_INS
                                                                          ,P_Tab_BENEF_PER(b).COD_BENEFICIO
                                                                          ,P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                                                                          ,V_SEQ_PERIODO
                                                                          ,V_Tab_Hist
                                                                          ,'A'); -- Indicador de Registro Antes da Manutenção (Registro ANTERIOR a Ser Comparado Após a Manutenção)

                CASE P_Tab_PERIODOS(a).FLG_ACAO
                      WHEN 'I' THEN -- Inclusão
                           BEGIN
                               SELECT NVL(MAX(NUM_SEQ_RAT),0) + 1
                               INTO   V_PROX_NUM_SEQ_RAT
                               FROM   TB_BENEFICIO_RATEIO_PENSAO
                               WHERE  COD_INS       = P_Tab_BENEF_PER(b).COD_INS
                               AND    COD_BENEFICIO = P_Tab_BENEF_PER(b).COD_BENEFICIO
                               AND    SEQ_PERIODO   = V_SEQ_PERIODO;
                           EXCEPTION
                                WHEN OTHERS THEN
                                    V_PROX_NUM_SEQ_RAT := 1;
                           END;

                           INSERT INTO TB_BENEFICIO_RATEIO_PENSAO
                           (
                             NUM_SEQ
                            ,COD_INS
                            ,COD_BENEFICIO
                            ,COD_IDE_CLI_BEN
                            ,SEQ_PERIODO
                            ,NUM_SEQ_RAT
                            ,DAT_INI_VIG
                            ,DAT_FIM_VIG
                            ,VAL_PERCENT_RATEIO
                            ,FLG_STATUS
                            ,COD_ORIGEM
                            ,DAT_ING
                            ,DAT_ULT_ATU
                            ,NOM_USU_ULT_ATU
                            ,NOM_PRO_ULT_ATU
                           )
                           VALUES
                           (
                            SEQ_EVENTOS_PENSAO_RATEIOS.NEXTVAL
                           ,P_Tab_BENEF_PER(b).COD_INS
                           ,P_Tab_BENEF_PER(b).COD_BENEFICIO
                           ,P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                           ,V_SEQ_PERIODO
                           ,V_PROX_NUM_SEQ_RAT
                           ,P_Tab_BENEF_PER(b).DAT_INI_VIG
                           ,P_Tab_BENEF_PER(b).DAT_FIM_VIG
                           ,P_Tab_BENEF_PER(b).VAL_PERCENT_RATEIO
                           ,P_Tab_BENEF_PER(b).FLG_STATUS
                           ,P_Tab_BENEF_PER(b).COD_ORIGEM
                           ,SYSDATE
                           ,SYSDATE
                           ,P_Tab_BENEF_PER(b).USU_PROC
                           ,V_NOME_PROC
                           );

                     WHEN 'A' THEN -- Alteração
                           UPDATE TB_BENEFICIO_RATEIO_PENSAO
                           SET    DAT_INI_VIG        = P_Tab_BENEF_PER(b).DAT_INI_VIG
                                 ,DAT_FIM_VIG        = P_Tab_BENEF_PER(b).DAT_FIM_VIG
                                 ,VAL_PERCENT_RATEIO = P_Tab_BENEF_PER(b).VAL_PERCENT_RATEIO
                                 ,COD_ORIGEM         = P_Tab_BENEF_PER(b).COD_ORIGEM
                                 ,DAT_ULT_ATU        = SYSDATE
                                 ,NOM_USU_ULT_ATU    = P_Tab_BENEF_PER(b).USU_PROC
                                 ,NOM_PRO_ULT_ATU    = V_NOME_PROC
                           WHERE  COD_INS            = P_Tab_BENEF_PER(b).COD_INS
                           AND    COD_BENEFICIO      = P_Tab_BENEF_PER(b).COD_BENEFICIO
                           AND    COD_IDE_CLI_BEN    = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                           AND    SEQ_PERIODO        = V_SEQ_PERIODO;

                     WHEN 'C' THEN -- Cancelamento
                           UPDATE TB_BENEFICIO_RATEIO_PENSAO
                           SET    FLG_STATUS      = P_Tab_BENEF_PER(b).FLG_STATUS
                                 ,COD_ORIGEM      = P_Tab_BENEF_PER(b).COD_ORIGEM
                                 ,DAT_ULT_ATU     = SYSDATE
                                 ,NOM_USU_ULT_ATU = P_Tab_BENEF_PER(b).USU_PROC
                                 ,NOM_PRO_ULT_ATU = V_NOME_PROC
                           WHERE  COD_INS         = P_Tab_BENEF_PER(b).COD_INS
                           AND    COD_BENEFICIO   = P_Tab_BENEF_PER(b).COD_BENEFICIO
                           AND    COD_IDE_CLI_BEN = P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                           AND    SEQ_PERIODO     = V_SEQ_PERIODO;
                END CASE;

                -- Histórico - Prepara Registro Após a Operação.
                PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_MONTA_HIST(P_Tab_BENEF_PER(b).COD_INS
                                                                          ,P_Tab_BENEF_PER(b).COD_BENEFICIO
                                                                          ,P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                                                                          ,V_SEQ_PERIODO
                                                                          ,V_Tab_Hist
                                                                          ,'D'); -- Indicador de Registro Depois da Manutenção (Registro POSTERIOR a Ser Comparado Após a Manutenção)

                -- Histórico - Grava o Resultado da Operação.
                PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_GRAVA_HIST
                (
                 P_Tab_BENEF_PER(b).COD_INS
                ,NULL
                ,P_Tab_BENEF_PER(b).COD_IDE_CLI_BEN
                ,NULL
                ,NULL
                ,P_Tab_BENEF_PER(b).COD_BENEFICIO
                ,V_NOM_ORIGEM
                ,P_Tab_BENEF_PER(b).USU_PROC
                ,V_NOME_PROC
                ,V_Tab_Hist
                ,P_Tab_PERIODOS(a).FLG_ACAO
                ,P_COD_ERRO
                ,P_MSG_ERRO
                );

                IF P_COD_ERRO <> 0 THEN
                    IF NOT P_COMMIT_EXTERNO THEN
                        ROLLBACK;
                    END IF;

                    RETURN;
                END IF;

            END IF;
        END LOOP;
    END LOOP;

    -- Procedimento Realizado com Sucesso
    P_COD_ERRO := 0;
    P_MSG_ERRO := ' ';

    IF NOT P_COMMIT_EXTERNO THEN
        COMMIT;
    END IF;

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

           IF NOT P_COMMIT_EXTERNO THEN
               ROLLBACK;
           END IF;

 END SP_BENEFICIO_RATEIO_PENSAO_GRAVA;


 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2 
 )
 IS
   V_DAT_INI_PERIODO  DATE;
   V_DAT_FIM_PERIODO  DATE;   
   V_EXISTE_REG       NUMBER;
   V_DES_ERRO         varchar2(4000);
   V_COD_ERRO         varchar2(4000);   
   V_SEQ_PERIODO      NUMBER;
   V_PROX_NUM_SEQ_RAT NUMBER;
   V_MAX_PERIODO      NUMBER;
   
   V_Tab_Hist        PAC_LOG_HIST_EVENTOS.t_Tab_Hist_BENEFICIO_RATEIO_PENSAO;
   
   V_COD_ORIGEM      TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE := 1; -- Fluxo
   V_NOM_ORIGEM      TB_HIST_EVENTO.NOM_ORIGEM%TYPE := 'TelaBeneficioRateio';
   
   V_NVL_DATA        DATe := TO_DATE('01/01/1900','dd/mm/yyyy');

 BEGIN   
    -- Maior Sequencia de periodo
    SELECT NVL(MAX(BRP.SEQ_PERIODO),0)
      INTO  V_MAX_PERIODO
      FROM  TB_BENEFICIO_RATEIO_PENSAO  BRP
     WHERE  COD_INS         = P_COD_INS
       AND  COD_BENEFICIO   = TO_NUMBER(P_COD_ADM_TRA);
     
    -- Cursor trazendo os beneficiarios que devem ser incluidos   
    FOR C_BENEF_UPF IN
    (SELECT DISTINCT  COD_INS
                     ,COD_ADM_TRA
                     ,COD_IDE_CLI_SERV
                     ,COD_IDE_CLI_BEN
                     ,DAT_INI_BEN
                     ,DAT_FIM_BEN_PREVISTA
                     ,PROPORCAO
                     ,DENSE_RANK() OVER(ORDER BY NPM.DAT_INI_BEN,DAT_FIM_BEN_PREVISTA) AS NUM_SEQ_PERIODO 
       FROM TB_NPM_BENEFICIARIO NPM
      WHERE COD_INS                = P_COD_INS
        AND TO_NUMBER(COD_ADM_TRA) = TO_NUMBER(P_COD_ADM_TRA)
        AND COD_REGISTRO           = P_COD_REGISTRO
        AND FLG_DEFERIR = 'S'  
        AND NOT EXISTS (SELECT '1'
                          FROM  TB_BENEFICIO_RATEIO_PENSAO BRP
                         WHERE  COD_INS         = NPM.COD_INS
                           AND  COD_BENEFICIO   = TO_NUMBER(NPM.COD_ADM_TRA)
                           AND  COD_IDE_CLI_BEN = NPM.COD_IDE_CLI_BEN)
      ORDER BY COD_IDE_CLI_BEN)
    LOOP      
         -- Setando período
         V_DAT_INI_PERIODO := C_BENEF_UPF.DAT_INI_BEN;
         V_DAT_FIM_PERIODO := C_BENEF_UPF.DAT_FIM_BEN_PREVISTA;                            
           
         -- Verifica número do periodo na tabela destino.
         -- Se Tiver usa oo mesmo número
         BEGIN   
           SELECT SEQ_PERIODO, MAX(BRP.NUM_SEQ_RAT)
             INTO V_SEQ_PERIODO, V_PROX_NUM_SEQ_RAT
             FROM TB_BENEFICIO_RATEIO_PENSAO BRP
            WHERE COD_INS           = C_BENEF_UPF.COD_INS
              AND BRP.COD_BENEFICIO = TO_NUMBER(C_BENEF_UPF.COD_ADM_TRA)
              AND BRP.DAT_INI_VIG   = V_DAT_INI_PERIODO
              AND NVL(BRP.DAT_FIM_VIG,V_NVL_DATA) = NVL(V_DAT_FIM_PERIODO,V_NVL_DATA)
             GROUP BY SEQ_PERIODO;
         EXCEPTION
           WHEN NO_DATA_FOUND THEN 
             V_SEQ_PERIODO      := C_BENEF_UPF.NUM_SEQ_PERIODO + V_MAX_PERIODO;   
             V_PROX_NUM_SEQ_RAT := 0;                
         END;               
         --        
         -- Histórico - Prepara Registro Antes da Operação.
         PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_MONTA_HIST(C_BENEF_UPF.COD_INS
                                                                    ,C_BENEF_UPF.COD_ADM_TRA
                                                                    ,C_BENEF_UPF.COD_IDE_CLI_BEN
                                                                    ,V_SEQ_PERIODO
                                                                    ,V_Tab_Hist
                                                                    ,'A'); -- Indicador de Registro Antes da Manutenção (Registro ANTERIOR a Ser Comparado Após a Manutenção)
         V_PROX_NUM_SEQ_RAT := V_PROX_NUM_SEQ_RAT +1;
         INSERT INTO TB_BENEFICIO_RATEIO_PENSAO
         (
          NUM_SEQ
         ,COD_INS
         ,COD_BENEFICIO
         ,COD_IDE_CLI_BEN
         ,SEQ_PERIODO
         ,NUM_SEQ_RAT
         ,DAT_INI_VIG
         ,DAT_FIM_VIG
         ,VAL_PERCENT_RATEIO
         ,FLG_STATUS
         ,COD_ORIGEM
         ,DAT_ING
         ,DAT_ULT_ATU
         ,NOM_USU_ULT_ATU
         ,NOM_PRO_ULT_ATU
        )
        VALUES
        (
         SEQ_EVENTOS_PENSAO_RATEIOS.NEXTVAL
        ,C_BENEF_UPF.COD_INS
        ,C_BENEF_UPF.COD_ADM_TRA
        ,C_BENEF_UPF.COD_IDE_CLI_BEN
        ,V_SEQ_PERIODO
        ,V_PROX_NUM_SEQ_RAT
        ,V_DAT_INI_PERIODO
        ,V_DAT_FIM_PERIODO 
        ,C_BENEF_UPF.PROPORCAO
        ,'V'
        ,V_COD_ORIGEM
        ,SYSDATE
        ,SYSDATE
        ,P_USU_PROC
        ,V_NOME_PROC
        );
     
        -- Histórico - Prepara Registro Após a Operação.
        PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_MONTA_HIST(C_BENEF_UPF.COD_INS
                                                                  ,C_BENEF_UPF.COD_ADM_TRA
                                                                 ,C_BENEF_UPF.COD_IDE_CLI_BEN
                                                                 ,V_SEQ_PERIODO
                                                                 ,V_Tab_Hist
                                                                 ,'D'); -- Indicador de Registro Depois da Manutenção (Registro POSTERIOR a Ser Comparado Após a Manutenção)

        -- Histórico - Grava o Resultado da Operação.
        PAC_LOG_HIST_EVENTOS.SP_BENEFICIO_RATEIO_PENSAO_GRAVA_HIST
        (
         C_BENEF_UPF.COD_INS
        ,NULL
        ,C_BENEF_UPF.COD_IDE_CLI_BEN
        ,NULL
        ,NULL
        ,C_BENEF_UPF.COD_ADM_TRA
        ,V_NOM_ORIGEM
        ,P_USU_PROC
        ,V_NOME_PROC
        ,V_Tab_Hist
        ,'I' -- Inclusão
        ,P_COD_ERRO
        ,P_MSG_ERRO
        );
        IF P_COD_ERRO <> 0 THEN
           ROLLBACK;
           RETURN;
        END IF;
    END LOOP;       
    
    COMMIT;

    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

           ROLLBACK;

 END SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO;
 --
 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_INCLUSAO
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
 )
 IS
   V_DAT_INI_PERIODO  DATE;
   V_DAT_FIM_PERIODO  DATE;   
   V_EXISTE_REG       NUMBER;
   V_DES_ERRO         varchar2(4000);
   V_COD_ERRO         varchar2(4000);   
   V_SEQ_PERIODO      NUMBER;
   V_PROX_NUM_SEQ_RAT NUMBER;
   V_MAX_PERIODO      NUMBER;
   V_COD_BENEFICIO    NUMBER;
   V_COD_BENEFICIO_ASSOC NUMBER;
   V_PROP             NUMBER;
   V_NUM_SEQ_RAT      NUMBER;
   V_NUM_SEQ_PERIODO  NUMBER;
   V_DAT_INI_MIN      DATE;
   V_DAT_FIM_MAX      DATE;
   DAT_VIG_PREV       DATE;
   
   V_Tab_Hist        PAC_LOG_HIST_EVENTOS.t_Tab_Hist_BENEFICIO_RATEIO_PENSAO;
   
   V_COD_ORIGEM      TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE := 1; -- Fluxo
   V_NOM_ORIGEM      TB_HIST_EVENTO.NOM_ORIGEM%TYPE := 'TelaBeneficioRateio';
   
   V_NVL_DATA        DATE := TO_DATE('01/01/2200','dd/mm/yyyy');
   V_EXISTE BOOLEAN := FALSE;
   
   PROCEDURE INSERE_RATEIO(I_COD_INS                      TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE,
                           I_COD_BENEFICIO                TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE,
                           I_COD_IDE_CLI_BEN              TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE,
                           I_DAT_INI_VIG                  TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE,
                           I_DAT_FIM_VIG                  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE,
                           I_VAL_PERCENT_RATEIO           TB_BENEFICIO_RATEIO_PENSAO.VAL_PERCENT_RATEIO%TYPE)         
   AS
     V_NUM_SEQ     TB_BENEFICIO_RATEIO_PENSAO.NUM_SEQ%TYPE;
   BEGIN 
     ---
     V_NUM_SEQ         := SEQ_EVENTOS_PENSAO_RATEIOS.NEXTVAL;
     V_NUM_SEQ_RAT     := V_NUM_SEQ_RAT +1;
     V_NUM_SEQ_PERIODO := V_NUM_SEQ_PERIODO + 1;
     --   
     --     
     INSERT INTO TB_BENEFICIO_RATEIO_PENSAO
     (NUM_SEQ,
      COD_INS,
      COD_BENEFICIO,
      COD_IDE_CLI_BEN,
      SEQ_PERIODO,
      NUM_SEQ_RAT,
      DAT_INI_VIG,
      DAT_FIM_VIG,
      VAL_PERCENT_RATEIO,
      COD_ORIGEM,
      DAT_ING,
      DAT_ULT_ATU,
      FLG_STATUS,
      NOM_USU_ULT_ATU,
      NOM_PRO_ULT_ATU     
     )
     values
     (V_NUM_SEQ,           --NUM_SEQ,
      I_COD_INS,           --COD_INS,
      I_COD_BENEFICIO,     --COD_BENEFICIO,
      I_COD_IDE_CLI_BEN,   --COD_IDE_CLI_BEN,
      V_NUM_SEQ_PERIODO,   --SEQ_PERIODO,
      V_NUM_SEQ_RAT,       --NUM_SEQ_RAT,
      I_DAT_INI_VIG,       --DAT_INI_VIG,
      I_DAT_FIM_VIG,       --DAT_FIM_VIG,
      I_VAL_PERCENT_RATEIO,--VAL_PERCENT_RATEIO,
      1,                   --COD_ORIGEM,
      sysdate,             --DAT_ING,
      sysdate,             --DAT_ULT_ATU,
      'T',                 --FLG_STATUS,
      P_USU_PROC,          --NOM_USU_ULT_ATU,
      'INSERE_RATEIO'      --NOM_PRO_ULT_ATU
     );
     --               
   END INSERE_RATEIO;                              

 BEGIN   
    --
    V_COD_BENEFICIO := TO_NUMBER(P_COD_ADM_TRA);
    --
    SELECT CLI.CLI_COD_ADM_TRA_ASSOC
      INTO V_COD_BENEFICIO_ASSOC
      FROM WRKCLI CLI
     WHERE CLI.CLI_COD_INS = P_COD_INS 
       AND CLI.CLI_COD_ADM_TRA = P_COD_ADM_TRA;
    --
    -- Maior Sequencia de periodo
    SELECT NVL(MAX(BRP.NUM_SEQ_RAT),0),
           NVL(MAX(BRP.SEQ_PERIODO),0)
      INTO V_NUM_SEQ_RAT , V_NUM_SEQ_PERIODO
      FROM TB_BENEFICIO_RATEIO_PENSAO  BRP
     WHERE COD_INS         = P_COD_INS
       AND COD_BENEFICIO   = V_COD_BENEFICIO_ASSOC;
    --
    --Pegando menor registro           
    --               
   
    -- Cursor trazendo os beneficiarios que devem ser incluidos   
    FOR C_BENEF_NPM IN
    (SELECT DISTINCT  COD_INS
                     ,COD_ADM_TRA
                     ,COD_IDE_CLI_SERV
                     ,COD_IDE_CLI_BEN
                     ,DAT_INI_BEN
                     ,DAT_FIM_BEN_PREVISTA
                     ,PROPORCAO
                     ,FLG_RESERVA_COTA
       FROM TB_NPM_BENEFICIARIO NPM
      WHERE COD_INS                = P_COD_INS
        AND TO_NUMBER(COD_ADM_TRA) = V_COD_BENEFICIO
        AND COD_REGISTRO                = (SELECT COD_REGISTRO FROM
                                        TB_NPM_BENEFICIARIO NPM2
                                        WHERE NUM_SEQ IN(
                                       ( SELECT MAX(NPM3.NUM_SEQ)
                                          FROM TB_NPM_BENEFICIARIO NPM3
                                         WHERE NPM3.COD_INS = P_COD_INS
                                           AND TO_NUMBER(NPM3.COD_ADM_TRA)= V_COD_BENEFICIO)))
        AND FLG_DEFERIR = 'S'  
        and NUM_SEQ_NPM_IMPORTADO is null
        AND NOT EXISTS (SELECT '1'
                          FROM  TB_BENEFICIO_RATEIO_PENSAO BRP
                         WHERE  COD_INS         = NPM.COD_INS
                           AND  COD_BENEFICIO   = V_COD_BENEFICIO_ASSOC
                           AND  COD_IDE_CLI_BEN = NPM.COD_IDE_CLI_BEN)   
                         ORDER BY COD_IDE_CLI_BEN)
    LOOP      
         V_EXISTE := TRUE;     
          
         -- Selecione os beneficio que estao concomitantes 
         UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
            SET BRP.FLG_STATUS = 'T'
          WHERE BRP.COD_INS = P_COD_INS
            AND BRP.COD_BENEFICIO= V_COD_BENEFICIO_ASSOC
            AND BRP.FLG_STATUS = 'V'
            AND BRP.DAT_INI_VIG < NVL(C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,V_NVL_DATA)
            AND NVL(BRP.DAT_FIM_VIG,V_NVL_DATA) > C_BENEF_NPM.DAT_INI_BEN;
            
         IF C_BENEF_NPM.FLG_RESERVA_COTA IS NULL THEN            
           -- PARTE 1: QUEBRA O PERIODO DOS RATEIOS JA EXISTENTES NA TB_BENEFICIO_RATEIO_PENSAO
           -- 
           -- Inicia com selecao dos beneficios que cruzam com a data inicio
           FOR C_BENEF_DET IN
           (SELECT BRP.ROWID AS LINHA,
                   BRP.COD_IDE_CLI_BEN,
                   BRP.DAT_FIM_VIG,
                   BRP.VAL_PERCENT_RATEIO
              FROM TB_BENEFICIO_RATEIO_PENSAO BRP
             WHERE BRP.COD_INS = P_COD_INS
               AND BRP.FLG_STATUS = 'T'
               AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
               AND C_BENEF_NPM.DAT_INI_BEN > BRP.DAT_INI_VIG)
           LOOP
             --
             UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
                SET BRP.DAT_FIM_VIG = C_BENEF_NPM.DAT_INI_BEN - 1,
                    BRP.FLG_STATUS = 'V' --PERIODO NÃO CONCOMITANTE
              WHERE BRP.ROWID = C_BENEF_DET.LINHA;
              --
              INSERE_RATEIO(P_COD_INS, 
                            V_COD_BENEFICIO_ASSOC, 
                            C_BENEF_DET.COD_IDE_CLI_BEN,
                            C_BENEF_NPM.DAT_INI_BEN,
                            C_BENEF_DET.DAT_FIM_VIG,
                            C_BENEF_DET.VAL_PERCENT_RATEIO);
              --                                      
           END LOOP; 
           --  selecao dos beneficios que se encontram no meio do periodo
           FOR C_BENEF_DET IN
           (SELECT BRP.ROWID AS LINHA,
                   BRP.COD_IDE_CLI_BEN,
                   BRP.DAT_FIM_VIG
              FROM TB_BENEFICIO_RATEIO_PENSAO BRP
             WHERE BRP.COD_INS = P_COD_INS
               AND BRP.FLG_STATUS = 'T'
               AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
               AND nvl(BRP.DAT_FIM_VIG,V_NVL_DATA) between C_BENEF_NPM.DAT_INI_BEN and NVL(C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,V_NVL_DATA))
           LOOP
             -- pega rateio do beneficiario
             /*SELECT NPM.PROPORCAO
               INTO V_PROP             
               FROM TB_NPM_BENEFICIARIO NPM
              WHERE NPM.COD_INS = p_COD_INS
                AND NPM.COD_ADM_TRA = V_COD_BENEFICIO
                AND NPM.COD_IDE_CLI_BEN = C_BENEF_DET.COD_IDE_CLI_BEN;
             --  
             UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
                SET BRP.VAL_PERCENT_RATEIO = V_PROP
              WHERE BRP.ROWID = C_BENEF_DET.LINHA; */
              NULL;
              --                                      
           END LOOP;                                                               
           -- Inicia com selecao dos beneficios que cruzam com a data fim                                    
           -- Cenário apenas de data fim do novo servidor não for nulo
           IF C_BENEF_NPM.DAT_FIM_BEN_PREVISTA is not null then 
             FOR C_BENEF_DET IN
             (SELECT BRP.ROWID AS LINHA,
                     BRP.COD_IDE_CLI_BEN,
                     BRP.DAT_INI_VIG,
                     BRP.VAL_PERCENT_RATEIO
                FROM TB_BENEFICIO_RATEIO_PENSAO BRP
               WHERE BRP.COD_INS = P_COD_INS
                 AND BRP.FLG_STATUS = 'T'
                 AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
                 AND nvl(BRP.DAT_FIM_VIG,V_NVL_DATA) > C_BENEF_NPM.DAT_FIM_BEN_PREVISTA)
             LOOP
               -- pega rateio do beneficiario
               SELECT NPM.PROPORCAO
                 INTO V_PROP             
                 FROM TB_NPM_BENEFICIARIO NPM
                WHERE NPM.COD_INS = p_COD_INS
                  AND NPM.COD_ADM_TRA = V_COD_BENEFICIO
                  AND NPM.COD_IDE_CLI_BEN = C_BENEF_DET.COD_IDE_CLI_BEN;
               --
               UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
                  SET BRP.DAT_INI_VIG = C_BENEF_NPM.DAT_FIM_BEN_PREVISTA + 1,          
                      BRP.FLG_STATUS = 'V'
                WHERE BRP.ROWID = C_BENEF_DET.LINHA;
                --
                INSERE_RATEIO(P_COD_INS, 
                              V_COD_BENEFICIO_ASSOC, 
                              C_BENEF_DET.COD_IDE_CLI_BEN,
                              C_BENEF_DET.DAT_INI_VIG,
                              C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,
                              C_BENEF_DET.VAL_PERCENT_RATEIO);
               --                                      
             END LOOP;   
           END IF;
         END IF;                  
         --    
         -- ATUALIZA RATEIOS 
         UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
            SET BRP.VAL_PERCENT_RATEIO = C_BENEF_NPM.PROPORCAO
          WHERE BRP.COD_INS = P_COD_INS
            AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC             
            AND BRP.FLG_STATUS = 'T';
         --       
         -- PARTE 2: QUEBRA O PERIODO DO beneficiario novo que sera incluidos NA TB_BENEFICIO_RATEIO_PENSAO E INSERE         
         FOR C_BENEF_PER IN
           (SELECT DISTINCT DAT_VIG 
              FROM (SELECT BRP.DAT_INI_VIG AS DAT_VIG
                      FROM TB_BENEFICIO_RATEIO_PENSAO BRP
                     WHERE BRP.COD_INS = P_COD_INS
                       AND BRP.FLG_STATUS = 'T'
                       AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
                     UNION
                    SELECT BRP.Dat_Fim_Vig AS DAT_VIG 
                      FROM TB_BENEFICIO_RATEIO_PENSAO BRP
                     WHERE BRP.COD_INS = P_COD_INS
                       AND BRP.FLG_STATUS = 'T'
                       AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
                     UNION
                    SELECT C_BENEF_NPM.DAT_INI_BEN AS DAT_VIG
                      FROM DUAL
                     UNION
                    SELECT C_BENEF_NPM.DAT_FIM_BEN_PREVISTA AS DAT_VIG
                      FROM DUAL
                   )
              ORDER BY NVL(DAT_VIG,V_NVL_DATA)
           )
         LOOP
           --
           IF DAT_VIG_PREV IS NULL THEN 
              DAT_VIG_PREV :=  C_BENEF_PER.DAT_VIG;
              CONTINUE;                            
           END IF;
           --
           INSERE_RATEIO(P_COD_INS, 
                         V_COD_BENEFICIO_ASSOC, 
                         C_BENEF_NPM.COD_IDE_CLI_BEN,
                         DAT_VIG_PREV,
                         C_BENEF_PER.DAT_VIG,
                         C_BENEF_NPM.PROPORCAO);
           --
           DAT_VIG_PREV :=  C_BENEF_PER.DAT_VIG+1;                         
           --
         END LOOP;   
         --                                                                            
      END LOOP;  
      --
      -- PARTE 3 - Atualiza numero sequencial de período      
      IF V_EXISTE THEN        
        DECLARE
           V_QTQ_EE NUMBER;
           v_per number;
         BEGIN 
           SELECT COUNT(1)
             INTO V_QTQ_EE
             FROM TB_BENEFICIO_RATEIO_PENSAO brp
            WHERE BRP.COD_INS = P_COD_INS
              AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
              and brp.flg_status ='T';
            
           if V_QTQ_EE = 2 then  v_per := 50;
           elsif  V_QTQ_EE = 3 then  v_per := 33;   
           elsif  V_QTQ_EE = 4 then  v_per := 25;   
           end if;
             
           UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
              SET BRP.VAL_PERCENT_RATEIO = v_per
            WHERE BRP.COD_INS = P_COD_INS
              AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC             
              AND BRP.FLG_STATUS = 'T';           
              
           UPDATE TB_BENEFICIARIO BEN
              SET BEN.VAL_PERCENTUAL = v_per
            WHERE BEN.COD_INS = P_COD_INS
              AND BEN.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC;            
                      
         END; 
        --
        -- update necessario para alterar valor sequencial crescente dos periodos
        -- devido a chave, incluindo um novo período que engloba 
        UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP 
          SET SEQ_PERIODO = SEQ_PERIODO +  V_NUM_SEQ_PERIODO +1        
        WHERE BRP.COD_INS = P_COD_INS
           AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC;
        --    
        FOR C_1 in (
        SELECT BRP.ROWID AS LINHA,
               DENSE_RANK() OVER (ORDER BY BRP.DAT_INI_VIG,BRP.DAT_FIM_VIG) AS NUM_SEQ_PERIODO 
          FROM TB_BENEFICIO_RATEIO_PENSAO BRP
         WHERE BRP.COD_INS = P_COD_INS
           AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC)       
        LOOP                
          UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP 
             SET SEQ_PERIODO = C_1.NUM_SEQ_PERIODO         
           WHERE ROWID =  C_1.LINHA;              
        END LOOP;  
      END IF;   
      --
      --
      UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
         SET BRP.FLG_STATUS = 'V'
       WHERE BRP.COD_INS = P_COD_INS
         AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
         AND BRP.FLG_STATUS = 'T';
      --
      --
      P_COD_ERRO := 0;                                                                    
    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

           ROLLBACK;

 END SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_INCLUSAO;
 --
 --
 PROCEDURE SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_RESERVA
 (
    P_COD_INS      IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
   ,P_COD_ADM_TRA  IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE
   ,P_COD_REGISTRO IN  TB_NPM_BENEFICIARIO.COD_REGISTRO%TYPE
   ,P_USU_PROC     IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE
   ,P_COD_ERRO     OUT NUMBER
   ,P_MSG_ERRO     OUT VARCHAR2
 )
 IS
   V_DAT_INI_PERIODO  DATE;
   V_DAT_FIM_PERIODO  DATE;   
   V_EXISTE_REG       NUMBER;
   V_DES_ERRO         varchar2(4000);
   V_COD_ERRO         varchar2(4000);   
   V_SEQ_PERIODO      NUMBER;
   V_PROX_NUM_SEQ_RAT NUMBER;
   V_MAX_PERIODO      NUMBER;
   V_COD_BENEFICIO    NUMBER;
   V_COD_BENEFICIO_ASSOC NUMBER;
   V_PROP             NUMBER;
   V_NUM_SEQ_RAT      NUMBER;
   V_NUM_SEQ_PERIODO  NUMBER;
   V_DAT_INI_MIN      DATE;
   V_DAT_FIM_MAX      DATE;
   DAT_VIG_PREV       DATE;
   v_qtd              NUMBER;
   
   V_Tab_Hist        PAC_LOG_HIST_EVENTOS.t_Tab_Hist_BENEFICIO_RATEIO_PENSAO;
   
   V_COD_ORIGEM      TB_BENEFICIO_RATEIO_PENSAO.COD_ORIGEM%TYPE := 1; -- Fluxo
   V_NOM_ORIGEM      TB_HIST_EVENTO.NOM_ORIGEM%TYPE := 'TelaBeneficioRateio';
   
   V_NVL_DATA        DATE := TO_DATE('01/01/2200','dd/mm/yyyy');
   V_EXISTE BOOLEAN := FALSE;
   
   PROCEDURE INSERE_RATEIO(I_COD_INS                      TB_BENEFICIO_RATEIO_PENSAO.COD_INS%TYPE,
                           I_COD_BENEFICIO                TB_BENEFICIO_RATEIO_PENSAO.COD_BENEFICIO%TYPE,
                           I_COD_IDE_CLI_BEN              TB_BENEFICIO_RATEIO_PENSAO.COD_IDE_CLI_BEN%TYPE,
                           I_DAT_INI_VIG                  TB_BENEFICIO_RATEIO_PENSAO.DAT_INI_VIG%TYPE,
                           I_DAT_FIM_VIG                  TB_BENEFICIO_RATEIO_PENSAO.DAT_FIM_VIG%TYPE,
                           I_VAL_PERCENT_RATEIO           TB_BENEFICIO_RATEIO_PENSAO.VAL_PERCENT_RATEIO%TYPE)         
   AS
     V_NUM_SEQ     TB_BENEFICIO_RATEIO_PENSAO.NUM_SEQ%TYPE;

   BEGIN 
     ---
     V_NUM_SEQ         := SEQ_EVENTOS_PENSAO_RATEIOS.NEXTVAL;
     V_NUM_SEQ_RAT     := V_NUM_SEQ_RAT +1;
     V_NUM_SEQ_PERIODO := V_NUM_SEQ_PERIODO + 1;
     --   
     --     
     INSERT INTO TB_BENEFICIO_RATEIO_PENSAO
     (NUM_SEQ,
      COD_INS,
      COD_BENEFICIO,
      COD_IDE_CLI_BEN,
      SEQ_PERIODO,
      NUM_SEQ_RAT,
      DAT_INI_VIG,
      DAT_FIM_VIG,
      VAL_PERCENT_RATEIO,
      COD_ORIGEM,
      DAT_ING,
      DAT_ULT_ATU,
      FLG_STATUS,
      NOM_USU_ULT_ATU,
      NOM_PRO_ULT_ATU     
     )
     values
     (V_NUM_SEQ,           --NUM_SEQ,
      I_COD_INS,           --COD_INS,
      I_COD_BENEFICIO,     --COD_BENEFICIO,
      I_COD_IDE_CLI_BEN,   --COD_IDE_CLI_BEN,
      V_NUM_SEQ_PERIODO,   --SEQ_PERIODO,
      V_NUM_SEQ_RAT,       --NUM_SEQ_RAT,
      I_DAT_INI_VIG,       --DAT_INI_VIG,
      I_DAT_FIM_VIG,       --DAT_FIM_VIG,
      I_VAL_PERCENT_RATEIO,--VAL_PERCENT_RATEIO,
      1,                   --COD_ORIGEM,
      sysdate,             --DAT_ING,
      sysdate,             --DAT_ULT_ATU,
      'T',                 --FLG_STATUS,
      user,                --NOM_USU_ULT_ATU,
      P_USU_PROC           --NOM_PRO_ULT_ATU
     );
     
   END INSERE_RATEIO;                              

 BEGIN   
    select count(1)
      into v_qtd
      from TB_NPM_BENEFICIARIO NPM
      where NPM.cod_INS = P_COD_INS
        AND NPM.COD_ADM_TRA = P_COD_ADM_TRA
        AND NPM.FLG_RESERVA_COTA = 'S';
    
    if v_qtd > 0 then 
      P_COD_ERRO := 1;
      P_MSG_ERRO := 'Já existe Reserva de Cota para o(s) Beneficiario(s).';    
      return;
    end if;       
    --
    V_COD_BENEFICIO := TO_NUMBER(P_COD_ADM_TRA);
    --
    SELECT CLI.CLI_COD_ADM_TRA_ASSOC
      INTO V_COD_BENEFICIO_ASSOC
      FROM WRKCLI CLI
     WHERE CLI.CLI_COD_INS = P_COD_INS 
       AND CLI.CLI_COD_ADM_TRA = P_COD_ADM_TRA;
    --
    -- Maior Sequencia de periodo
    SELECT NVL(MAX(BRP.NUM_SEQ_RAT),0),
           NVL(MAX(BRP.SEQ_PERIODO),0)
      INTO V_NUM_SEQ_RAT , V_NUM_SEQ_PERIODO
      FROM TB_BENEFICIO_RATEIO_PENSAO  BRP
     WHERE COD_INS         = P_COD_INS
       AND COD_BENEFICIO   = V_COD_BENEFICIO_ASSOC;
    --
    --Pegando menor registro           
    --                  
    -- Cursor trazendo os beneficiarios que devem ser incluidos   
    FOR C_BENEF_NPM IN
    (SELECT DISTINCT  COD_INS
                     ,COD_ADM_TRA
                     ,COD_IDE_CLI_SERV
                     ,COD_IDE_CLI_BEN
                     ,DAT_INI_BEN
                     ,DAT_FIM_BEN_PREVISTA
                     ,PROPORCAO
       FROM TB_NPM_BENEFICIARIO NPM
      WHERE COD_INS                = P_COD_INS
        AND TO_NUMBER(COD_ADM_TRA) = V_COD_BENEFICIO
        AND COD_REGISTRO                = (SELECT COD_REGISTRO FROM
                                        TB_NPM_BENEFICIARIO NPM2
                                        WHERE NUM_SEQ IN(
                                       ( SELECT MAX(NPM3.NUM_SEQ)
                                          FROM TB_NPM_BENEFICIARIO NPM3
                                         WHERE NPM3.COD_INS = P_COD_INS
                                           AND TO_NUMBER(NPM3.COD_ADM_TRA)= V_COD_BENEFICIO)))
        --AND FLG_DEFERIR = 'S'  
        and NUM_SEQ_NPM_IMPORTADO is null
        AND NOT EXISTS (SELECT '1'
                          FROM  TB_BENEFICIO_RATEIO_PENSAO BRP
                         WHERE  COD_INS         = NPM.COD_INS
                           AND  COD_BENEFICIO   = V_COD_BENEFICIO_ASSOC
                           AND  COD_IDE_CLI_BEN = NPM.COD_IDE_CLI_BEN)   
                         ORDER BY COD_IDE_CLI_BEN)
    LOOP      
         V_EXISTE := TRUE;     
         -- Selecione os beneficio que estao concomitantes 
         UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
            SET BRP.FLG_STATUS = 'T'
          WHERE BRP.COD_INS = P_COD_INS
            AND BRP.COD_BENEFICIO= V_COD_BENEFICIO_ASSOC
            AND BRP.FLG_STATUS = 'V'
            AND BRP.DAT_INI_VIG < NVL(C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,V_NVL_DATA)
            AND NVL(BRP.DAT_FIM_VIG,V_NVL_DATA) > C_BENEF_NPM.DAT_INI_BEN;
         -- PARTE 1: QUEBRA O PERIODO DOS RATEIOS JA EXISTENTES NA TB_BENEFICIO_RATEIO_PENSAO
         -- 
         -- Inicia com selecao dos beneficios que cruzam com a data inicio
         FOR C_BENEF_DET IN
         (SELECT BRP.ROWID AS LINHA,
                 BRP.COD_IDE_CLI_BEN,
                 BRP.DAT_FIM_VIG,
                 BRP.VAL_PERCENT_RATEIO
            FROM TB_BENEFICIO_RATEIO_PENSAO BRP
           WHERE BRP.COD_INS = P_COD_INS
             AND BRP.FLG_STATUS = 'T'
             AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
             AND C_BENEF_NPM.DAT_INI_BEN > BRP.DAT_INI_VIG)
         LOOP
           --
           UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
              SET BRP.DAT_FIM_VIG = C_BENEF_NPM.DAT_INI_BEN - 1,
                  BRP.FLG_STATUS = 'V' --PERIODO NÃO CONCOMITANTE
            WHERE BRP.ROWID = C_BENEF_DET.LINHA;
            --
            INSERE_RATEIO(P_COD_INS, 
                          V_COD_BENEFICIO_ASSOC, 
                          C_BENEF_DET.COD_IDE_CLI_BEN,
                          C_BENEF_NPM.DAT_INI_BEN,
                          C_BENEF_DET.DAT_FIM_VIG,
                          C_BENEF_DET.VAL_PERCENT_RATEIO);
            --                                      
         END LOOP; 
         --  selecao dos beneficios que se encontram no meio do periodo
         FOR C_BENEF_DET IN
         (SELECT BRP.ROWID AS LINHA,
                 BRP.COD_IDE_CLI_BEN,
                 BRP.DAT_FIM_VIG
            FROM TB_BENEFICIO_RATEIO_PENSAO BRP
           WHERE BRP.COD_INS = P_COD_INS
             AND BRP.FLG_STATUS = 'T'
             AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
             AND nvl(BRP.DAT_FIM_VIG,V_NVL_DATA) between C_BENEF_NPM.DAT_INI_BEN and NVL(C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,V_NVL_DATA))
         LOOP
           -- pega rateio do beneficiario
           /*SELECT NPM.PROPORCAO
             INTO V_PROP             
             FROM TB_NPM_BENEFICIARIO NPM
            WHERE NPM.COD_INS = p_COD_INS
              AND NPM.COD_ADM_TRA = V_COD_BENEFICIO
              AND NPM.COD_IDE_CLI_BEN = C_BENEF_DET.COD_IDE_CLI_BEN;
           --  
           UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
              SET BRP.VAL_PERCENT_RATEIO = V_PROP
            WHERE BRP.ROWID = C_BENEF_DET.LINHA; */
            NULL;
            --                                      
         END LOOP;                                                               
         -- Inicia com selecao dos beneficios que cruzam com a data fim                                    
         -- Cenário apenas de data fim do novo servidor não for nulo
         IF C_BENEF_NPM.DAT_FIM_BEN_PREVISTA is not null then 
           FOR C_BENEF_DET IN
           (SELECT BRP.ROWID AS LINHA,
                   BRP.COD_IDE_CLI_BEN,
                   BRP.DAT_INI_VIG,
                   BRP.VAL_PERCENT_RATEIO
              FROM TB_BENEFICIO_RATEIO_PENSAO BRP
             WHERE BRP.COD_INS = P_COD_INS
               AND BRP.FLG_STATUS = 'T'
               AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
               AND nvl(BRP.DAT_FIM_VIG,V_NVL_DATA) > C_BENEF_NPM.DAT_FIM_BEN_PREVISTA)
           LOOP
             -- pega rateio do beneficiario
             SELECT NPM.PROPORCAO
               INTO V_PROP             
               FROM TB_NPM_BENEFICIARIO NPM
              WHERE NPM.COD_INS = p_COD_INS
                AND NPM.COD_ADM_TRA = V_COD_BENEFICIO
                AND NPM.COD_IDE_CLI_BEN = C_BENEF_DET.COD_IDE_CLI_BEN;
             --
             UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
                SET BRP.DAT_INI_VIG = C_BENEF_NPM.DAT_FIM_BEN_PREVISTA + 1,          
                    BRP.FLG_STATUS = 'V'
              WHERE BRP.ROWID = C_BENEF_DET.LINHA;
              --
              INSERE_RATEIO(P_COD_INS, 
                            V_COD_BENEFICIO_ASSOC, 
                            C_BENEF_DET.COD_IDE_CLI_BEN,
                            C_BENEF_DET.DAT_INI_VIG,
                            C_BENEF_NPM.DAT_FIM_BEN_PREVISTA,
                            C_BENEF_DET.VAL_PERCENT_RATEIO);
             --                                      
           END LOOP;   
         END IF;
         -- ATUALIZA RATEIOS 
            UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
               SET BRP.VAL_PERCENT_RATEIO = C_BENEF_NPM.PROPORCAO
             WHERE BRP.COD_INS = P_COD_INS
               AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC             
               AND BRP.FLG_STATUS = 'T';

         --       
         -- PARTE 2: QUEBRA O PERIODO DO beneficiario novo que sera incluidos NA TB_BENEFICIO_RATEIO_PENSAO E INSERE         
     /*    FOR C_BENEF_PER IN
           (SELECT DISTINCT DAT_VIG 
              FROM (SELECT BRP.DAT_INI_VIG AS DAT_VIG
                      FROM TB_BENEFICIO_RATEIO_PENSAO BRP
                     WHERE BRP.COD_INS = P_COD_INS
                       AND BRP.FLG_STATUS = 'T'
                       AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
                     UNION
                    SELECT BRP.Dat_Fim_Vig AS DAT_VIG 
                      FROM TB_BENEFICIO_RATEIO_PENSAO BRP
                     WHERE BRP.COD_INS = P_COD_INS
                       AND BRP.FLG_STATUS = 'T'
                       AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
                     UNION
                    SELECT C_BENEF_NPM.DAT_INI_BEN AS DAT_VIG
                      FROM DUAL
                     UNION
                    SELECT C_BENEF_NPM.DAT_FIM_BEN_PREVISTA AS DAT_VIG
                      FROM DUAL
                   )
              ORDER BY NVL(DAT_VIG,V_NVL_DATA)
           )
         LOOP
           --
           IF DAT_VIG_PREV IS NULL THEN 
              DAT_VIG_PREV :=  C_BENEF_PER.DAT_VIG;
              CONTINUE;                            
           END IF;
           --
           INSERE_RATEIO(P_COD_INS, 
                         V_COD_BENEFICIO_ASSOC, 
                         C_BENEF_NPM.COD_IDE_CLI_BEN,
                         DAT_VIG_PREV,
                         C_BENEF_PER.DAT_VIG,
                         C_BENEF_NPM.PROPORCAO);
           --
           DAT_VIG_PREV :=  C_BENEF_PER.DAT_VIG+1;                         
           --
         END LOOP;   */
         --                                                          
      END LOOP;  
      --
      -- PARTE 3 - Atualiza numero sequencial de período      
      IF V_EXISTE THEN 
        --
        -- update necessario para alterar valor sequencial crescente dos periodos
        -- devido a chave, incluindo um novo período que engloba 
        UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP 
          SET SEQ_PERIODO = SEQ_PERIODO +  V_NUM_SEQ_PERIODO +1        
        WHERE BRP.COD_INS = P_COD_INS
           AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC;
        --    
        FOR C_1 in (
        SELECT BRP.ROWID AS LINHA,
               DENSE_RANK() OVER (ORDER BY BRP.DAT_INI_VIG,BRP.DAT_FIM_VIG) AS NUM_SEQ_PERIODO 
          FROM TB_BENEFICIO_RATEIO_PENSAO BRP
         WHERE BRP.COD_INS = P_COD_INS
           AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC)       
        LOOP                
          UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP 
             SET SEQ_PERIODO = C_1.NUM_SEQ_PERIODO         
           WHERE ROWID =  C_1.LINHA;              
        END LOOP;  
      END IF;   
      --
      --
      UPDATE TB_BENEFICIO_RATEIO_PENSAO BRP
         SET BRP.FLG_STATUS = 'V'
       WHERE BRP.COD_INS = P_COD_INS
         AND BRP.COD_BENEFICIO = V_COD_BENEFICIO_ASSOC
         AND BRP.FLG_STATUS = 'T';
      --
      --
      UPDATE TB_NPM_BENEFICIARIO NPM
         SET NPM.FLG_RESERVA_COTA = 'S'
       WHERE NPM.COD_INS = P_COD_INS
         AND NPM.COD_ADM_TRA = V_COD_BENEFICIO
         AND NPM.NUM_SEQ_NPM_IMPORTADO IS NULL;
     
      P_COD_ERRO := 0;                                                                    
    EXCEPTION
       WHEN OTHERS THEN
           P_COD_ERRO := SQLCODE;
           P_MSG_ERRO := SQLERRM;

           ROLLBACK;

 END SP_BENEFICIO_RATEIO_PENSAO_GRAVA_RATEIOS_FLUXO_RESERVA;

 -- Funções
 FUNCTION FN_VALOR_BENEFICIO
 (
   P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
  ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
  ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
  ,P_VALOR            IN  NUMBER
  ,P_COD_ERRO         OUT NUMBER
  ,P_MSG_ERRO         OUT VARCHAR2
 )
 RETURN NUMBER
 IS

 V_VALOR  NUMBER;

 BEGIN
     V_VALOR := FN_BENEFICIO_RATEIO_PENSAO_VALOR_BENEFICIO(P_COD_INS
                                                          ,P_COD_BENEFICIO
                                                          ,P_COD_IDE_CLI_BEN
                                                          ,P_VALOR
                                                          ,P_COD_ERRO
                                                          ,P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
        RETURN V_VALOR;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     RETURN V_VALOR;

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END FN_VALOR_BENEFICIO;


 FUNCTION FN_VALIDA_RATEIO_NOVA_LEI
 (
   P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
  ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
  ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
  ,P_COD_ERRO         OUT NUMBER
  ,P_MSG_ERRO         OUT VARCHAR2
 )
 RETURN BOOLEAN
 IS

 V_VALIDA  BOOLEAN;

 BEGIN
     V_VALIDA := FN_BENEFICIO_RATEIO_PENSAO_VALIDA_RATEIO_NOVA_LEI(P_COD_INS
                                                                  ,P_COD_BENEFICIO
                                                                  ,P_COD_IDE_CLI_BEN
                                                                  ,P_COD_ERRO
                                                                  ,P_MSG_ERRO);
     IF P_COD_ERRO <> 0 THEN
        RETURN V_VALIDA;
     END IF;

     -- Procedimento Realizado com Sucesso
     P_COD_ERRO := 0;
     P_MSG_ERRO := ' ';

     RETURN V_VALIDA;

     EXCEPTION
         WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;

 END FN_VALIDA_RATEIO_NOVA_LEI;


 FUNCTION FN_BENEFICIO_RATEIO_PENSAO_VALOR_BENEFICIO
 (
   P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
  ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
  ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
  ,P_VALOR            IN  NUMBER
  ,P_COD_ERRO         OUT NUMBER
  ,P_MSG_ERRO         OUT VARCHAR2
 )
 RETURN NUMBER
 IS

 PAR_PERIODO   DATE;
 PAR_COD_TIPO  CHAR(3);
 TOT_BRUTO     NUMBER(16,4);

 BEGIN
     BEGIN
          -- Procedimento Realizado com Sucesso
          P_COD_ERRO  := 0;
          P_MSG_ERRO  := ' ';

           BEGIN
              SELECT
                   BEN.PER_ULT_PROCESSO,
                   DECODE(CON.COD_TIPO_BENEFICIO,'M','PEN','APO')
                   INTO PAR_PERIODO ,
                        PAR_COD_TIPO
              FROM TB_BENEFICIARIO BEN, TB_CONCESSAO_BENEFICIO CON
              WHERE BEN.COD_INS         = P_COD_INS        AND
                    BEN.COD_BENEFICIO   = P_COD_BENEFICIO  AND
                    BEN.COD_IDE_CLI_BEN = P_COD_IDE_CLI_BEN AND
                    CON.COD_INS         = P_COD_INS        AND
                    CON.COD_BENEFICIO   = P_COD_BENEFICIO;
           END;

           CASE
                  WHEN     ( P_COD_BENEFICIO>=40000000 AND P_COD_BENEFICIO <41000000 )
                        OR ( P_COD_BENEFICIO>=50000000 AND P_COD_BENEFICIO  <51000000 )
                        OR
                        (P_COD_BENEFICIO>=80000000 AND P_COD_BENEFICIO<81000000 ) THEN
                  TOT_BRUTO:=  PAC_AUDIT_FOLHA.FN_OBTEM_HBRUTO (1, to_char(PAR_PERIODO,'dd/mm/rrrr'),1,'N','APO' , P_COD_BENEFICIO,P_COD_IDE_CLI_BEN);
               ELSE
                  TOT_BRUTO:=  PAC_AUDIT_FOLHA.FN_OBTEM_HBRUTO (1, to_char(PAR_PERIODO,'dd/mm/rrrr'),1,'N','PEN' , P_COD_BENEFICIO,P_COD_IDE_CLI_BEN);
            END CASE;

         RETURN TOT_BRUTO;

     EXCEPTION
          WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;
     END;

 END FN_BENEFICIO_RATEIO_PENSAO_VALOR_BENEFICIO;


 FUNCTION FN_BENEFICIO_RATEIO_PENSAO_VALIDA_RATEIO_NOVA_LEI
 (
   P_COD_INS          IN  TB_CONCESSAO_BENEFICIO.COD_INS%TYPE
  ,P_COD_BENEFICIO    IN  TB_CONCESSAO_BENEFICIO.COD_BENEFICIO%TYPE
  ,P_COD_IDE_CLI_BEN  IN  TB_BENEFICIARIO.COD_IDE_CLI_BEN%TYPE
  ,P_COD_ERRO         OUT NUMBER
  ,P_MSG_ERRO         OUT VARCHAR2
 )
 RETURN BOOLEAN
 IS

 V_EXISTE_REG  NUMBER(1);

 BEGIN
     BEGIN
        BEGIN
             SELECT  1
             INTO    V_EXISTE_REG
             FROM    TB_BENEFICIO_RATEIO_PENSAO
             WHERE   COD_INS         = P_COD_INS
             AND     COD_BENEFICIO   = P_COD_BENEFICIO
             AND     COD_IDE_CLI_BEN = P_COD_IDE_CLI_BEN
             AND    (DAT_FIM_VIG IS NULL -- Vigente
                     OR
                    (DAT_FIM_VIG >= SYSDATE AND FLG_STATUS = 'V')) -- Período Futuro Vigente
             AND    ROWNUM = 1;
         EXCEPTION
             WHEN OTHERS THEN
                 V_EXISTE_REG := 0;
         END;

         -- Procedimento Realizado com Sucesso
         P_COD_ERRO  := 0;
         P_MSG_ERRO  := ' ';

         IF V_EXISTE_REG = 1 THEN
             RETURN TRUE;
         ELSE
             RETURN FALSE;
         END IF;

     EXCEPTION
          WHEN OTHERS THEN
             P_COD_ERRO := SQLCODE;
             P_MSG_ERRO := SQLERRM;
             RETURN FALSE;
     END;

 END FN_BENEFICIO_RATEIO_PENSAO_VALIDA_RATEIO_NOVA_LEI;

END PAC_EVENTOS_RATEIOS_NPM;
/
