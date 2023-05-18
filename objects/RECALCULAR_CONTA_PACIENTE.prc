create or replace 
procedure recalcular_conta_paciente (	nr_interno_conta_p		number,
				nm_usuario_p		varchar2) is

dt_atualizacao_w    	date		:= sysdate;
ie_classificacao_w      	number(1)	:= 0;
cd_item_w	      	number(6)	:= 0;
cd_convenio_w	      	number(10)	:= 0;
vl_item_w	      		number(13,2)	:= 0;
vl_material_w	      	number(13,2)	:= 0;
vl_final_w	      		number(13,2)	:= 0;
qt_item_w	      	number(13,4)	:= 0;
ie_erro_w	      		varchar2(1) 	:= 'N';
nr_sequencia_w	      	number(10)	:= 0;
ie_tipo_convenio_w      	number(2)	:= 0;
ie_status_acerto_w		number(1);
nr_atendimento_w		number(10,0)	:= 0;
qt_reg_procedimento_w	number(10)	:= 0;
qt_reg_material_w		number(10)	:= 0;
ie_tipo_atendimento_w   	number(3);
qt_pacote_w		number(10)	:= 0;
ie_cancelamento_w		varchar2(1);
dt_hora_inicial_w		date;
dt_hora_final_w		date;
nr_seq_recalculo_w		number(10);
nr_seq_protocolo_w	number(10);
cd_cep_w		Varchar2(15);
cd_municipio_ibge_w	Varchar2(6);
ds_endereco_w		Varchar2(100);
nr_aih_w			Number(13);
nr_seq_aih_w		Number(10);
qt_proc_unif_w		Number(10)	:= 0;
cd_estabelecimento_w	number(4,0);
ie_recalcular_fora_pacote_w	varchar2(1);
nr_seq_proc_interno_w	number(10);
cd_procedimento_tuss_w	number(15);
nr_seq_exame_w		number(10);

ie_repasse_proc_conv_w	Varchar2(255);
ie_repasse_mat_conv_w	Varchar2(255);
ie_repasse_w		Varchar2(255);

ie_calculo_taxa_regra_w	varchar2(01)	:= 'C';
nr_seq_regra_preco_w	Number(10,0);
cd_categoria_w		varchar2(10);
cd_edicao_amb_w		varchar2(240);
ie_regra_tipo_atend_w	varchar2(1):= 'A';
qt_rateio_sp_bpa_w	Number(10);
nr_seq_material_w		number(10);
ie_momento_rateio_sh_w	varchar2(15) := 'P';
qt_reg_adic_w		number(10);
ie_excluir_conta_vazia_w	varchar2(1):= 'S';
cd_setor_atendimento_w	procedimento_paciente.cd_setor_atendimento%type;
nr_seq_atepacu_w		procedimento_paciente.nr_seq_atepacu%type;
cd_tipo_acomod_unid_w	tipo_acomodacao.cd_tipo_acomodacao%type;
ie_complexidade_w		sus_procedimento.ie_complexidade%type;
qt_reg_drg_w		number;
qt_param_taxa_w		number;
cd_proc_real_w		sus_aih_unif.cd_procedimento_real%type;
dt_inicial_w		sus_aih_unif.dt_inicial%type;
qt_sessions_w		number;
nr_regra_w      number;
ie_recalcular_adto_conta_w	varchar2(1);
ie_existem_registros_w          number;

vl_desconto_w conta_paciente_desc_item.vl_desconto%type := 0;

CURSOR C00 IS
      select  a.nr_sequencia
        from  procedimento_paciente a
        where a.nr_interno_conta   = nr_interno_conta_p
	   and	a.tx_procedimento <> 100
	   and	a.ie_valor_informado = 'N'
	   and	(((ie_tipo_atendimento_w = 1) and (a.ie_origem_proced = 7)) or
	   	(a.ie_origem_proced = 2))
	  order by a.nr_sequencia;

CURSOR C01 IS
      select  	a.nr_sequencia,
		nvl(a.cd_convenio,x.cd_convenio_parametro),
		nvl(a.cd_categoria, x.cd_categoria_parametro),
              		b.ie_classificacao,
		a.nr_seq_proc_interno,
		a.nr_seq_exame,
		a.cd_edicao_amb,
		a.nr_seq_material,
		a.cd_setor_atendimento,
		a.nr_seq_atepacu
        from  	procedimento_paciente a,
		procedimento b,
		conta_paciente x
        where 	a.cd_procedimento  = b.cd_procedimento
          and 	a.ie_origem_proced = b.ie_origem_proced
          and 	x.nr_interno_conta = a.nr_interno_conta
	  and	x.nr_interno_conta = nr_interno_conta_p
	  and 	a.nr_seq_proc_pacote is null
	  order by a.nr_sequencia;

CURSOR C02 IS
      select	nr_sequencia
        from  	material_atend_paciente
        where 	nr_interno_conta  = nr_interno_conta_p
	and 	nr_seq_proc_pacote is null
	  order by  nr_sequencia;

CURSOR C03 IS
	select	cd_item,
		sum(qt_item),
		sum(vl_item)
	from	conta_paciente_v
	where	nr_interno_conta  = nr_interno_conta_p
	and	cd_motivo_exc_conta is null
	and	ie_proc_mat = 2
	and	qt_item <> 0
	and	(ie_tipo_convenio_w <> 3)
	group by cd_item;

Cursor C04 is
	select	a.nr_sequencia
	from	procedimento_paciente a,
		sus_valor_proc_paciente b
	where	a.nr_interno_conta  = nr_interno_conta_p
	and	a.nr_sequencia = b.nr_sequencia
	and	a.cd_motivo_exc_conta is null
	and	sus_validar_regra(11, cd_proc_real_w, 7,dt_inicial_w) = 0
	and	(nvl(b.cd_registro_proc,Sus_Obter_TipoReg_Proc(a.cd_procedimento, a.ie_origem_proced, 'C', 2)) = '3')
	and	a.cd_Procedimento	<> cd_proc_real_w;

Cursor C05 is
	select	a.nr_sequencia nr_seq_propaci,
		null nr_seq_matpaci,
		a.ie_red_val_original,
		b.vl_procedimento,
		null vl_material
	from	conta_pac_ded_conv_item a,
		procedimento_paciente b,
		conta_pac_deducao_conv d
	where	d.nr_seq_conta_orig = nr_interno_conta_p
	and	a.nr_seq_deducao_conv = d.nr_sequencia
	and	b.nr_sequencia = a.nr_seq_propaci_origem 
        and     nvl(b.ie_valor_informado, 'N') = 'N'
	UNION ALL
	select	null nr_seq_propaci,
		a.nr_sequencia nr_seq_matpaci,
		a.ie_red_val_original,
		null vl_procedimento,
		c.vl_material
	from	conta_pac_ded_conv_item a,
		conta_pac_deducao_conv d,
		material_atend_paciente c
	where	d.nr_seq_conta_orig = nr_interno_conta_p
	and	a.nr_seq_deducao_conv = d.nr_sequencia
	and	c.nr_sequencia = a.nr_seq_matpaci_origem
        and     nvl(c.ie_valor_informado, 'N') = 'N';

type 		fetch_array is table of c05%rowtype;
s_array 	fetch_array;
i		integer := 1;
type vetor is table of fetch_array index by binary_integer;
vetor_c05_w	vetor;

BEGIN
dt_hora_inicial_w		:= sysdate;

Calcula_regra_tx_pck.set_calcula_regra_tx('N');

/* Recalculo de procedimentos e servicos */
begin

if (nvl(pkg_i18n.get_user_locale, 'pt_BR') = 'es_AR') then
    processar_conta_arg(nr_interno_conta_p, nm_usuario_p, 'S', 'S');
        select	count(a.nr_sequencia)
	into	ie_existem_registros_w 
	from	conta_pac_deducao_conv a
	where  	a.nr_seq_conta_orig = nr_interno_conta_p;
	
	if (ie_existem_registros_w > 0) then
		open c05;
		loop
		fetch c05 bulk collect into s_array limit 1000;
			vetor_c05_w(i) := s_array;
			i := i + 1;
		exit when c05%notfound;
		end loop;
		close c05;
	end if;
end if;

select	b.ie_tipo_convenio,
	a.ie_status_acerto,
	c.nr_atendimento,
	c.ie_tipo_atendimento,
	nvl(a.ie_cancelamento,'X'),
	nvl(a.cd_estabelecimento, c.cd_estabelecimento)
into	ie_tipo_convenio_w,
	ie_status_acerto_w,
	nr_atendimento_w,
	ie_tipo_atendimento_w,
	ie_cancelamento_w,
	cd_estabelecimento_w
from	conta_paciente a,
	atendimento_paciente c,
	convenio b
where	nvl(a.cd_convenio_calculo,a.cd_convenio_parametro) 	= b.cd_convenio
and	a.nr_interno_conta	= nr_interno_conta_p
and	a.nr_atendimento	= c.nr_atendimento;
exception
	when too_many_rows then
	ie_tipo_convenio_w := 0;
	when no_data_found then
	ie_tipo_convenio_w := 0;
end;

ie_recalcular_fora_pacote_w	:= obter_valor_param_usuario(67, 290, obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w);
ie_excluir_conta_vazia_w	:= nvl(obter_valor_param_usuario(67, 675, obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w),'S');

--Setar package em modo de recalculo da conta
Param_Recalculo_Conta_pck.set_recalcular_conta('S');

--Pre -Carregar parametros de materiais para a package (Atualiza_preco_material)
Param_Recalculo_Conta_pck.set_parametro(24,99,obter_perfil_ativo, nm_usuario_p, 0, '1');
Param_Recalculo_Conta_pck.set_parametro(24,103,obter_perfil_ativo, nm_usuario_p, 0, 'N');
Param_Recalculo_Conta_pck.set_parametro(67,637,obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w, 'S');
Param_Recalculo_Conta_pck.set_parametro(67,500,obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w, 'N');
Param_Recalculo_Conta_pck.set_parametro(67,370,obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w, 'N');
Param_Recalculo_Conta_pck.set_parametro(42,67,obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w, 'N');

/*Obter parametros de Faturamento*/
select	nvl(max(ie_calculo_taxa_regra),'C'),
	nvl(max(ie_regra_tipo_atend),'A')
into	ie_calculo_taxa_regra_w,
	ie_regra_tipo_atend_w
from	parametro_faturamento
where	cd_estabelecimento	= cd_estabelecimento_w;

/* Verificar se conta possue pacote */
qt_pacote_w := 0;
begin
select	count(1)
into	qt_pacote_w
from	procedimento_paciente
where	nr_interno_conta 	= nr_interno_conta_p
and	nr_sequencia = nr_seq_proc_pacote
and	rownum = 1;
exception
when no_data_found then
	qt_pacote_w := 0;
end;

/* Verificar se a conta e do SUS Unificado */
begin
select	count(1)
into	qt_proc_unif_w
from	procedimento_paciente
where	nr_interno_conta	= nr_interno_conta_p
and	cd_motivo_exc_conta is null
and	ie_origem_proced	= 7
and	rownum	= 1;
exception
when no_data_found then
	qt_proc_unif_w := 0;
end;

select max(1)
into   nr_regra_w
from   regra_exclusao_proc
where  rownum = 1
and    cd_convenio in (select cd_convenio_parametro 
                       from   conta_paciente 
                       where  nr_interno_conta = nr_interno_conta_p)
and    ie_situacao = 'A';

if (nr_regra_w > 0) then 
  gerar_procedimento_relacionado(nr_interno_conta_p, nm_usuario_p);
end if; 

if	(ie_tipo_convenio_w in (3,10)) then
	OPEN 	C00;
	LOOP
   		FETCH C00 into nr_sequencia_w;
    		exit when C00%NOTFOUND;

		update procedimento_paciente
		set tx_procedimento	= 100
		where nr_sequencia	= nr_sequencia_w;

		if	(qt_proc_unif_w = 0) then
			atualiza_preco_proc_sus(nr_sequencia_w, nm_usuario_p);
		end if;
	END LOOP;
	CLOSE C00;

	if	(qt_proc_unif_w = 0) then
		Atualizar_taxa_aih (nr_interno_conta_p);
	else
		Sus_Atualiza_Taxa_Proc_Esp(nr_interno_conta_p, nm_usuario_p);
	end if;

	if	(nvl(ie_regra_tipo_atend_w,'A') = 'C') then
		begin
		select	nvl(max(ie_tipo_atend_conta),ie_tipo_atendimento_w)
		into	ie_tipo_atendimento_w
		from 	conta_paciente
		where 	nr_interno_conta = nr_interno_conta_p;
		exception
			when no_data_found then
			ie_tipo_atendimento_w := ie_tipo_atendimento_w;
		end;

	end if;

	if (ie_tipo_atendimento_w = 1) then
			select nvl(max(sus_obter_complexidade_aih(nr_interno_conta_p)),'AC')
			into ie_complexidade_w
			from dual;

			update sus_aih_unif
			set ie_complexidade = ie_complexidade_w
			where nr_interno_conta = nr_interno_conta_p
			and	ie_complexidade <> ie_complexidade_w;
	end if;
end if;

gerar_taxa_procedimento(nr_atendimento_w, nr_interno_conta_p, nm_usuario_p);
				       
select	max(nr_seq_protocolo)
into	nr_seq_protocolo_w
from	conta_paciente
where	nr_interno_conta = nr_interno_conta_p;

limpa_doc_conv_conta(nr_interno_conta_p, nr_seq_protocolo_w, ie_status_acerto_w);

if	((ie_recalcular_fora_pacote_w = 'S') or (qt_pacote_w = 0)) then
	begin
	OPEN 	C01;
	LOOP
   	FETCH C01 into
		nr_sequencia_w,
		cd_convenio_w,
		cd_categoria_w,
		ie_classificacao_w,
		nr_seq_proc_interno_w,
		nr_seq_exame_w,
		cd_edicao_amb_w,
		nr_seq_material_w,
		cd_setor_atendimento_w,
		nr_seq_atepacu_w;
    	if 	C01%FOUND then
	       	if	(ie_classificacao_w = 1) then
			begin
			cd_procedimento_tuss_w:= 0;
			Atualiza_Preco_Procedimento(nr_sequencia_w, cd_convenio_w,	nm_usuario_p);
			if	(nvl(nr_seq_proc_interno_w, 0) > 0) then
				begin

				select	nvl(max(cd_tipo_acomodacao),0)
				into	cd_tipo_acomod_unid_w
				from 	atend_paciente_unidade a
				where	a.nr_atendimento = nr_atendimento_w
				and	nr_seq_interno = nr_seq_atepacu_w;

				cd_procedimento_tuss_w:= Define_procedimento_TUSS(cd_estabelecimento_w,  nr_seq_proc_interno_w, cd_convenio_w, cd_categoria_w,
	  						ie_tipo_atendimento_w, sysdate, 0, 0, somente_numero(cd_edicao_amb_w), cd_setor_atendimento_w, cd_tipo_acomod_unid_w);
				/*select	max(cd_procedimento_tuss)
				into	cd_procedimento_tuss_w
				from	proc_interno
				where	nr_sequencia = nr_seq_proc_interno_w;*/
				end;
			end if;
			if	(nvl(nr_seq_exame_w, 0) > 0) then
				cd_procedimento_tuss_w := Define_proc_TUSS_exame(nr_seq_exame_w,cd_convenio_w, cd_categoria_w, nr_seq_material_w, substr(obter_somente_numero(cd_edicao_amb_w),1,6));
			end if;
			if	(nvl(cd_procedimento_tuss_w, 0) > 0) then
				begin
				update	procedimento_paciente
				set	cd_procedimento_tuss = cd_procedimento_tuss_w
				where	nr_sequencia = nr_sequencia_w;
				--and	nvl(cd_procedimento_tuss, 0) = 0;
				end;
			end if;
			end;
		else
		/*Criado IF abaixo para a OS 198413, pois o cliente lanca um procedimento AMB, em uma conta de um convenio do tipo SUS,
		neste caso a Atualiza_Preco_Procedimento zera o valor do procedimento*/
			if	(ie_tipo_convenio_w = 3) then
				Atualiza_Preco_Servico(nr_sequencia_w, nm_usuario_p);
			else
				Atualiza_Preco_Procedimento(nr_sequencia_w, cd_convenio_w,	nm_usuario_p); --OS 194651 - Heckmann 10/02/2010 troquei o Atualiza_preco_Servico por este
			end if;
	       	end if;
    	else
		exit;
    	end if;
	END LOOP;
	CLOSE C01;
	end;
end if;

/* Recalculo de materiais e medicamentos */
if	((ie_recalcular_fora_pacote_w = 'S') or (qt_pacote_w = 0)) and
	(((ie_status_acerto_w = 2) and (ie_tipo_convenio_w <> 3)) or
	(ie_status_acerto_w = 1))	then
	begin
	OPEN 	C02;
	LOOP
	FETCH C02 into
		nr_sequencia_w;
    	if 	C02%FOUND then
       	Atualiza_Preco_Material
            	           (nr_sequencia_w,
                  	      nm_usuario_p);
    	else
       	exit;
    	end if;
	END LOOP;
	CLOSE C02;
	end;
end if;

if	(ie_status_acerto_w = 2) and
	(ie_tipo_convenio_w = 3) then
	begin
	OPEN 	C02;
	LOOP
	FETCH C02 into
		nr_sequencia_w;
    	if 	C02%FOUND then
       	atualiza_conta_contab_material
            	           (nr_sequencia_w,
                  	      nm_usuario_p);
    	else
       	exit;
    	end if;
	END LOOP;
	CLOSE C02;
	end;
end if;

/* Ajuste dos valores */
if	(qt_pacote_w = 0) then
	begin
	OPEN 	C03;
	LOOP
   	FETCH C03 	into
             	cd_item_w,
             	qt_item_w,
             	vl_item_w;
    	if 	C03%FOUND then
		begin
	 	ie_erro_w 		:= 'N';
	       	if   	(qt_item_w = 0)  and
			(vl_item_w <> 0) then
			begin
			select vl_material,
				 nr_sequencia
			into	 vl_material_w,
				 nr_sequencia_w
			from	 material_atend_paciente
			where	 nr_interno_conta 	= nr_interno_conta_p
			and	 cd_material	= cd_item_w
			and	 qt_material <> 0
			and	 dt_atendimento =
				 (select max(a.dt_atendimento)
					from 	material_atend_paciente a
					where	a.nr_interno_conta = nr_interno_conta_p
					and	a.cd_material	= cd_item_w
					and	a.qt_material <> 0);
			exception
     				when too_many_rows then
          			ie_erro_w 		:= 'S';
     				when no_data_found then
          			ie_erro_w 		:= 'S';
			end;
       		end if;
       		if   	(qt_item_w = 0)   and
			(vl_item_w <> 0)  and
			(ie_erro_w = 'N') then
			begin
			if	(vl_item_w < 0) then
				begin
				if	vl_material_w < 0 then
					vl_final_w := vl_material_w - vl_item_w;
				else
					vl_final_w := vl_material_w + vl_item_w;
				end if;
				end;
			else
				begin
				if	vl_material_w > 0 then
					vl_final_w := vl_material_w - vl_item_w;
				else
					vl_final_w := vl_material_w + vl_item_w;
				end if;
				end;
			end if;
			update	material_atend_paciente
			set		vl_material = vl_final_w
			where		nr_sequencia		= nr_sequencia_w;
			exception
     					when no_data_found then
          				ie_erro_w 		:= 'S';
			if (nvl(wheb_usuario_pck.get_ie_commit, 'S') = 'S') then commit; end if;
			end;
		end if;
		end;
    	else
       		exit;
    	end if;
	END LOOP;
	CLOSE C03;
	end;

end if;

select 	count(1)
into	qt_sessions_w
from	performed_sessions
where	cd_convenio = (	select	cd_convenio_parametro
			from	conta_paciente
			where	nr_interno_conta = nr_interno_conta_p);
			
if	(nvl(qt_sessions_w,0) > 0) then
	generate_acc_sessions(nr_interno_conta_p, nm_usuario_p);
end if;

select	count(*)
into	qt_reg_drg_w
from	parametro_drg
where	cd_estabelecimento = cd_estabelecimento_w;

if	(nvl(qt_reg_drg_w,0) > 0) then
	GERAR_TAXAS_CONTA_DRG(nr_interno_conta_p);
end if;

select	count(*)
into	qt_param_taxa_w
from	regra_taxa_conta;

if	(nvl(qt_param_taxa_w,0) > 0) then
	GERAR_TAXA_CONTA_FAT_PACIENTE(nr_interno_conta_p);
end if;

-- OS 211111
gerar_Taxa_Qtde_exec_valor(nr_interno_conta_p);

Gerar_conta_paciente_guia(nr_interno_conta_p, ie_status_acerto_w);

if	(ie_status_acerto_w = 2) 	then
	begin
	if	(ie_tipo_convenio_w = 3) then
		begin
		if	(nvl(ie_regra_tipo_atend_w,'A') = 'C') then
			begin
			select	nvl(max(ie_tipo_atend_conta),ie_tipo_atendimento_w)
			into	ie_tipo_atendimento_w
			from 	conta_paciente
			where 	nr_interno_conta = nr_interno_conta_p;
			exception
				when no_data_found then
				ie_tipo_atendimento_w := ie_tipo_atendimento_w;
			end;
		end if;

		if	(qt_proc_unif_w = 0) then
			begin
			Atualizar_classif_sus(nr_interno_conta_p);

			if	(ie_tipo_atendimento_w	= 1)		and
				(ie_cancelamento_w	= 'X')	then
				begin
				sus_limpar_proc_dif_aih(nr_interno_conta_p,nm_usuario_p);
				calcular_sus_porte_anestesico(nr_interno_conta_p);
				Atualizar_Valor_Ponto_SP(nr_atendimento_w, nr_interno_conta_p);
				end;
			end if;
			end;
		else
			begin
			if	(ie_tipo_atendimento_w	= 1) and
				(ie_cancelamento_w	= 'X') then
				begin
				cd_proc_real_w :=  Sus_Obter_Proc_Aih_Unif(nr_interno_conta_p, 2, 'C');

				begin
				select	dt_inicial 
				into	dt_inicial_w
				from	sus_aih_unif
				where	nr_interno_conta = nr_interno_conta_p;
				exception
				when no_data_found then
					begin	

					select 	dt_periodo_inicial
					into	dt_inicial_w
					from 	conta_paciente
					where	nr_interno_conta = nr_interno_conta_p;
					end;
				when others then
					begin
					dt_inicial_w := sysdate;
					end;
				end;
				/*Zerar Proc. Princ AIH que nao sao o Procedimento Real, pois os mesmos devem apenas entrar no Rateio*/
				open C04;
				loop
				fetch C04 into
					nr_sequencia_w;
				exit when C04%notfound;
					begin
					update	procedimento_paciente
					set	vl_materiais 		= 0,
						vl_medico		= 0,
						vl_custo_operacional	= 0,
						vl_procedimento		= 0
					where	nr_sequencia		= nr_sequencia_w;

					update	procedimento_participante
					set	vl_conta		= 0,
						vl_participante		= 0
					where 	nr_sequencia		= nr_sequencia_w;

					update	sus_valor_proc_paciente
					set	vl_matmed		= 0,
						vl_medico		= 0,
						vl_ato_anestesista	= 0,
						vl_sadt_rateado		= 0,
						vl_medico_rateio	= 0
					where	nr_sequencia		= nr_sequencia_w;
					end;
				end loop;
				close C04;

				Sus_Calcular_Valor_Anestesista(nr_interno_conta_p);
				Sus_Atualizar_Valor_SP(nr_atendimento_w, nr_interno_conta_p, nm_usuario_p);
				SUS_Gerar_incent_psiquiatrico(nr_atendimento_w, nr_interno_conta_p, nm_usuario_p);
				end;
			elsif	(ie_tipo_atendimento_w	<> 1) and
				(ie_cancelamento_w	= 'X') then
				begin
				select	count(1)
				into	qt_rateio_sp_bpa_w
				from	sus_regra_rateio_sp_bpa
				where	cd_estabelecimento = cd_estabelecimento_w
				and	ie_situacao = 'A'
				and	rownum = 1;

				if	(qt_rateio_sp_bpa_w > 0) then
					sus_rateio_sp_bpa(nr_interno_conta_p,cd_estabelecimento_w);
				end if;
				end;
			end if;

			end;
		end if;

		/* Edgar 13/05/2010, OS 215277, recalcular o repasse ao fechar a conta*/
		ie_repasse_proc_conv_w 	:= Obter_Valor_Conv_Estab(cd_convenio_w,cd_estabelecimento_w,'IE_REPASSE_PROC');
		ie_repasse_mat_conv_w	:= Obter_Valor_Conv_Estab(cd_convenio_w,cd_estabelecimento_w,'IE_REPASSE_MAT');

		ie_repasse_w := 'N';
		if 	((ie_repasse_proc_conv_w = 'C') or (ie_repasse_mat_conv_w = 'C')) then
			ie_repasse_w := 'S';
		end if;

		if	((ie_repasse_proc_conv_w is null) or (ie_repasse_mat_conv_w is null)) then
			select	nvl(max('S'),'N')
			into	ie_repasse_w
			from	parametro_faturamento
			where	cd_estabelecimento = cd_estabelecimento_w
			and	((ie_repasse_mat = 'C') or (ie_repasse_proc = 'C'));
		end if;

		if	(ie_repasse_w = 'S') then
			Recalcular_Conta_Repasse(nr_interno_conta_p, null, null, nm_usuario_p,null);
		end if;
		/* Edgar 13/05/2010, FIM OS 215277 */
		end;
	end if;

	if	(ie_tipo_convenio_w = 3) and
		(qt_proc_unif_w > 0) then
		begin

		if	(ie_tipo_atendimento_w	= 1) and
			(ie_cancelamento_w	= 'X') then
			begin

			select	nvl(max(ie_momento_rateio_sh),'P')
			into	ie_momento_rateio_sh_w
			from	sus_parametros_aih
			where	cd_estabelecimento	= cd_estabelecimento_w;

			if	(nvl(ie_momento_rateio_sh_w,'P') = 'C') then
				sus_atualiza_valor_sh_conta(nr_interno_conta_p, nm_usuario_p);
			end if;

			select	count(1)
			into	qt_reg_adic_w
			from	sus_regra_proc_adic_comp
			where	cd_estabelecimento = cd_estabelecimento_w
			and	ie_situacao = 'A'
			and	rownum = 1;

			if	(qt_reg_adic_w > 0) then
				begin
				sus_adiciona_valor_compl_proc(nr_interno_conta_p,cd_estabelecimento_w,nm_usuario_p);
				end;
			end if;

			end;
		end if;

		end;
	end if;

	Atualiza_Codigo_Convenio(nr_interno_conta_p,null);
	Atualizar_tabela_custo_conta(nr_interno_conta_p);
	Atualizar_Especialidade_conta(nr_interno_conta_p);
	Atualiza_Setor_Receita(nr_interno_conta_p);
	Atualizar_Resumo_Conta(nr_interno_conta_p, ie_status_acerto_w);

	end;
end if;

begin
    begin
        select  nvl(sum(cpdi.vl_desconto),0)
        into    vl_desconto_w
        from    conta_paciente_desconto cpd, conta_paciente_desc_item cpdi
        where   cpdi.nr_seq_desconto = cpd.nr_sequencia
        and     cpd.nr_interno_conta = nr_interno_conta_p;
    exception
        when no_data_found then
            vl_desconto_w := 0;
    end;

	update  conta_paciente
	set     dt_recalculo = sysdate,
            vl_desconto = vl_desconto_w
	where   nr_interno_conta = nr_interno_conta_p;
end;

processar_conta_drg_aus(nr_interno_conta_p,cd_estabelecimento_w,'S',nm_usuario_p);

/* Deletar contas sem procedimentos e materiais */
begin
select	count(1)
into	qt_reg_procedimento_w
from	procedimento_paciente
where	nr_interno_conta	= nr_interno_conta_p
and	rownum = 1;
exception
	when no_data_found then
		qt_reg_procedimento_w := 0;
end;

begin
select	count(1)
into	qt_reg_material_w
from	material_atend_paciente
where	nr_interno_conta = nr_interno_conta_p
and	rownum = 1;
exception
	when no_data_found then
		qt_reg_material_w := 0;
end;

if	(nvl(ie_excluir_conta_vazia_w,'S') = 'S') and
	(qt_reg_material_w + qt_reg_procedimento_w) = 0 then
	begin
	delete from conta_paciente
	where nr_interno_conta = nr_interno_conta_p
	and ((Obter_Tipo_Convenio(cd_convenio_parametro) <> 3) or
		(obter_se_contido_char(ds_observacao,WHEB_MENSAGEM_PCK.get_texto(298866)) = 'N'));
	exception
		when no_data_found then
			qt_reg_procedimento_w	:= 0;
	end;
end if;

/* Gravar conta paciente recalculo */
dt_hora_final_w	:= sysdate;
if	(qt_reg_material_w + qt_reg_procedimento_w) > 0 then
	begin
	select	conta_paciente_recalculo_seq.nextval
	into	nr_seq_recalculo_w
	from	dual;

	select	max(nr_seq_protocolo)
	into	nr_seq_protocolo_w
	from	conta_paciente
	where	nr_interno_conta	= nr_interno_conta_p;

	insert	into conta_paciente_recalculo(
 		NR_SEQUENCIA,
 		NR_INTERNO_CONTA,
 		DT_HORA_INICIAL,
 		DT_HORA_FINAL,
 		DT_ATUALIZACAO,
 		NM_USUARIO,
 		NR_SEQ_PROTOCOLO)
	values
		(nr_seq_recalculo_w,
		nr_interno_conta_p,
		dt_hora_inicial_w,
		dt_hora_final_w,
		sysdate,
		nm_usuario_p,
		nr_seq_protocolo_w);
	end;
end if;

Calcula_regra_tx_pck.set_calcula_regra_tx('S');

if	(ie_calculo_taxa_regra_w = 'L') then

	select  max(a.nr_sequencia)
	into	nr_seq_regra_preco_w
	from  	procedimento_paciente a,
		procedimento b,
		conta_paciente x
	where 	a.cd_procedimento  = b.cd_procedimento
	and 	a.ie_origem_proced = b.ie_origem_proced
	and 	x.nr_interno_conta = a.nr_interno_conta
	and	x.nr_interno_conta = nr_interno_conta_p
	and	nvl(a.ie_valor_informado,'N') = 'N'
	and 	a.nr_seq_proc_pacote is null
	and 	b.ie_classificacao <> 1
	and 	nvl(a.nr_seq_regra_preco,0) > 0;

	if	(nvl(nr_seq_regra_preco_w,0) > 0) then
		calcular_regra_preco_taxa(nr_interno_conta_p, nr_seq_regra_preco_w, 1, nm_usuario_p);
	end if;

end if;

atualiza_tipo_cobranca(nr_interno_conta_p);

Param_Recalculo_Conta_pck.set_recalcular_conta('N');

ie_recalcular_adto_conta_w	:= obter_valor_param_usuario(67, 744, obter_perfil_ativo, nm_usuario_p, cd_estabelecimento_w);

if (nvl(ie_recalcular_adto_conta_w,'N') = 'S') then
	recalcular_adiantamento_conta(nr_interno_conta_p, nm_usuario_p);
end if;

if (nvl(pkg_i18n.get_user_locale, 'pt_BR') = 'es_AR') then
    processar_conta_arg(nr_interno_conta_p, nm_usuario_p, 'S');
    for i in 1..vetor_c05_w.count loop
		s_array := vetor_c05_w(i);
		for z in 1..s_array.count loop
		begin
		ajustar_valor_proc_mat_arg (
			nr_interno_conta_p	=>	nr_interno_conta_p,
			nr_nr_seq_propaci_p	=>	s_array(z).nr_seq_propaci,
			nr_seq_matpaci_p	=>	s_array(z).nr_seq_matpaci,
			ie_red_val_original_p	=>	s_array(z).ie_red_val_original,
			vl_procedimento_p	=>	s_array(z).vl_procedimento,
			vl_material_p		=>	s_array(z).vl_material		);
		end;
		end loop;
	end loop; 
end if;

if (nvl(wheb_usuario_pck.get_ie_commit, 'S') = 'S') then commit; end if;

end recalcular_conta_paciente;
/
