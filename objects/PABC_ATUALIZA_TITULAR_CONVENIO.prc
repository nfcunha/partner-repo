CREATE OR REPLACE procedure PABC_ATUALIZA_TITULAR_CONVENIO(
nm_usuario_p Varchar2,
cd_convenio_p number,
cd_categoria_p varchar2,
cd_pessoa_fisica_p varchar2,
cd_plano_convenio_p varchar2,
dt_inicio_vigencia_p date,
dt_fim_vigencia_p date,
dt_validade_carteira_p date,
cd_usuario_convenio_p varchar2 default null,
ie_tipo_conveniado_p number default null) is

nr_sequencia_w pessoa_titular_convenio.nr_sequencia%TYPE;
qt_pessoa_tit_w number(10);
nr_prioridade_w pessoa_titular_convenio.nr_prioridade%type;
ie_atualiza_titular_conv_w varchar2(1);
ie_insere_w varchar2(1) default 'S';

begin

if (nvl(cd_convenio_p,0) > 0) and
(cd_pessoa_fisica_p is not null) then

select pessoa_titular_convenio_seq.nextval
into nr_sequencia_w
from dual;

insert into pessoa_titular_convenio(
nr_sequencia,
dt_atualizacao_nrec,
nm_usuario_nrec,
dt_atualizacao,
nm_usuario,
cd_pessoa_fisica,
cd_pessoa_titular,
cd_convenio,
cd_categoria,
cd_plano_convenio,
dt_inicio_vigencia,
dt_fim_vigencia,
dt_validade_carteira,
cd_usuario_convenio,
ie_tipo_conveniado,
nr_prioridade)
values (
nr_sequencia_w,
sysdate,
nm_usuario_p,
sysdate,
nm_usuario_p,
cd_pessoa_fisica_p,
cd_pessoa_fisica_p,
cd_convenio_p,
cd_categoria_p,
cd_plano_convenio_p,
dt_inicio_vigencia_p,
dt_fim_vigencia_p,
dt_validade_carteira_p,
cd_usuario_convenio_p,
ie_tipo_conveniado_p,
nr_prioridade_w);

end if;
/* Nao pode ter commit, eh chamada em uma trigger.*/
end PABC_ATUALIZA_TITULAR_CONVENIO;
/