# Transferências federais para OSCs

Este projeto mapeia as transferências do Executivo federal brasileiro para organizações da sociedade civil (OSCs) entre 2001 e 2023, descrevendo sua trajetória orçamentária, os maiores beneficiários por ano e o valor médio e mediano transferido por CNPJ. A série usa como fonte a maior base orçamentária disponível sobre o tema, o que permite observar não apenas o volume agregado dos repasses, mas também sua concentração entre um número relativamente pequeno de organizações.

## Fonte dos dados

Os dados vêm da base **Siga Brasil**, do Senado Federal, já ingerida no banco de dados interno do Ipea (`portal_osc2`, tabela `tb_orcamento_def_v3`). O script não baixa nada da internet: ele se conecta diretamente a esse banco por consulta SQL. Os valores estão deflacionados a preços de maio de 2024 pelo INPC.

Reproduzir a coleta, portanto, exige acesso à rede interna do Ipea (VPN ou intranet) e credenciais de banco válidas — não é possível rodar o script fora desse ambiente. Quem não tiver esse acesso ainda pode reproduzir as análises a partir dos CSVs já salvos em `data/`.

## Como reproduzir

1. Defina suas credenciais de banco em `~/.Renviron` (nunca no código):
   ```
   IPEA_DB_HOST=psql12
   IPEA_DB_NAME=portal_osc2
   IPEA_DB_USER=seu_usuario
   IPEA_DB_PASSWORD=sua_senha
   ```
2. Abra `transferencias_osc.Rproj` no RStudio.
3. Rode `code/transferencias_osc.R`. O script consulta o banco, salva os CSVs em `data/` e os gráficos em `figures/`.

## Estrutura do repositório

```
transferencias_osc/
├── README.md              este arquivo
├── transferencias_osc.Rproj
├── .gitignore
├── code/
│   └── transferencias_osc.R   consulta o banco, processa e gera os gráficos
├── data/
│   ├── transferencia_osc.csv       total anual transferido
│   ├── top_transferencia_osc.csv   dez maiores CNPJs por ano
│   └── transf_media.csv            número de OSCs, valor médio e mediano por ano
└── figures/
    ├── g_transferencias.png             série anual de transferências, 2001-2023
    └── transferencias federais para OSCs.png
```

## Notas metodológicas

- Um CNPJ específico (`28719664000124`) é excluído das consultas de total e de valor médio — verificar no código a razão dessa exclusão antes de usar os agregados fora deste projeto.
- Os anos de 2001 e 2021 são excluídos do gráfico dos dez maiores CNPJs por ano.
