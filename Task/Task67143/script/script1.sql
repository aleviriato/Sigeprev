-------------------------------
-- Data criação: 01/07/2020
-- Data aplicação pré: 01/07/2020
-- Data aplicação prod: 
-------------------------------
insert into tb_definicao (COD_INS, COD_NUM, DES_DESCRICAO, FLG_PAR_GLOB_PART, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU)
values (0, 20014, 'Tarefas de wrokflow que podem interseccionar com outros fluxos. ', 'G', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL');

insert into tb_definicao (COD_INS, COD_NUM, DES_DESCRICAO, FLG_PAR_GLOB_PART, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU)
values (0, 20015, 'Tarefas de encerramento de fluxo para intersecção entre fluxos', 'G', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL');

insert into tb_codigo (COD_INS, COD_NUM, COD_PAR, DES_DESCRICAO, DES_DESCRICAO_CURTA, NUM_ORDEM, COD_COR_SIPREV, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_SONDA, FLG_VIGENTE)
values (0, 20015, 'NPI', '301', 'Encaminhar Requerimento', 3, '0', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL', 0, null);
 
insert into tb_codigo (COD_INS, COD_NUM, COD_PAR, DES_DESCRICAO, DES_DESCRICAO_CURTA, NUM_ORDEM, COD_COR_SIPREV, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_SONDA, FLG_VIGENTE)
values (0, 20014, 'NPI/364', 'Digitalização de Documentos', 'Digitalização de Documentos', 1, '0', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL', 0, null);

insert into tb_codigo (COD_INS, COD_NUM, COD_PAR, DES_DESCRICAO, DES_DESCRICAO_CURTA, NUM_ORDEM, COD_COR_SIPREV, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_SONDA, FLG_VIGENTE)
values (0, 20014, 'NPI/365', 'Despacho/Análise Técnica (Análise)', 'Digitalização de Documentos', 2, '0', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL', 0, null);

insert into tb_codigo (COD_INS, COD_NUM, COD_PAR, DES_DESCRICAO, DES_DESCRICAO_CURTA, NUM_ORDEM, COD_COR_SIPREV, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_SONDA, FLG_VIGENTE)
values (0, 20014, 'NPI/373', 'Despacho/Análise Técnica (Análise)', 'Digitalização de Documentos', 3, '0', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL', 0, null);

insert into tb_codigo (COD_INS, COD_NUM, COD_PAR, DES_DESCRICAO, DES_DESCRICAO_CURTA, NUM_ORDEM, COD_COR_SIPREV, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_SONDA, FLG_VIGENTE)
values (0, 20014, 'NPI/372', 'Publicacao DOE', 'Publicacao DOE', 3, '0', to_date('17-06-2021', 'dd-mm-yyyy'), to_date('17-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'MANUAL', 0, null);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 7, 'NPI', 373, 'NPM', '367', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 3', 'E', '''P''', null, 'N', 373, 2);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 2, 'NPI', 364, 'NPM', '365', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 1', 'N', '''T''', null, null, null, null);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 3, 'NPI', 365, 'NPM', '366,370,376,380,383', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 2', 'E', '''P''', null, null, null, null);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 6, 'NPI', 372, 'NPM', '367', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 5', 'E', '''T''', 5, 'N', 365.373, 2);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 4, 'NPI', 365, 'NPM', '367', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 3', 'E', '''P''', null, 'N', 365, 2);

insert into tb_wrk_regras_intersec (COD_INS, NUM_SEQ_REGRAS_INTERSEC, COD_TIPO_FLUXO, COD_TAREFA, COD_TIPO_FLUXO_ASSOC, DES_LISTA_TAREFAS_ASSOC, DAT_ING, DAT_ULT_ATU, NOM_USU_ULT_ATU, NOM_PRO_ULT_ATU, COD_CONDICAO, DES_LISTA_STATUS, NUM_SEQ_REGRAS_INTERSEC_PAI, FLG_TIPO_FLUXO_OPC, DES_LISTA_TAREFAS_OPC, VAL_TAREFA_OPC)
values ('1', 5, 'NPI', 372, 'NPM', '375,377', to_date('21-06-2021', 'dd-mm-yyyy'), to_date('21-06-2021', 'dd-mm-yyyy'), 'AVIRIATO', 'CENARIO 5', 'N', '''T''', null, 'N', 365.373, 2);

