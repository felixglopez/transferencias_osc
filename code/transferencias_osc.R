Via linguagem de programação R
# instalação do pacote RPostgres
library("RPostgres")
library(DBI)
# conectar ao banco de dados
con <- dbConnect(RPostgres::Postgres(),dbname = 'portal_osc2', 
                 host = 'psql12', 
                 port = 5432,
                 user = '',
                 password = 'd')
#nota: incluir o comando bigint = ‘integer’ (con <- dbConnect(RPostgres::Postgres(),dbname = 'siape', bigint = 'integer',) evita o formato integer64 que o ggplot nao reconhece.

# realizar consulta
res <- dbGetQuery(con, "SELECT nr_orcamento_ano, SUM (nr_vl_empenhado_def_inpc_junho_24)
FROM public.tb_orcamento_def_v3
where nr_orcamento_cnpj != 28719664000124
GROUP BY nr_orcamento_ano 
ORDER BY nr_orcamento_ano ASC
;")
                   
# # realizar loop e imprimir os resultados
# while(!dbHasCompleted(res)){
#         chunk <- dbFetch(res, n = 5)
#         print(nrow(chunk))
# }
# finalizar conexão com o servidor (banco de dados)
dbDisconnect(con)

# para salvar o resultado da consulta em formato .csv
# a função é write.csv2(tabela, "nomedatabela.csv", fileEncoding = "UTF-8").  # Lembrar de definir a pasta de trabalho (ctrl+shift+h) antes. É nela que a   # tabela será salva.A função salva automaticamente com o ";" como separador.  # Exemplo:
write.csv2(res,"data/transferencia_osc.csv", fileEncoding = "UTF-8")

transferencia <- res

head(transferencia)

transferencia <- transferencia %>% 
        dplyr::rename(ano = nr_orcamento_ano,
                      total = sum)

library(dplyr)
library(ggplot2)
library(scales)

#convertendo os valores para bilhões
transferencia <- transferencia %>%
        mutate(total_bilhoes = total / 1e9,
               total_bilhoes_formatado = number(total_bilhoes, accuracy = 0.01, suffix = " bi"))


#criando um gráfico delinha com os valores
g_transferencias <- ggplot(data = transferencia, aes(x = ano, y = total_bilhoes)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = transferencia$ano) +
        scale_y_continuous(labels = label_number(suffix = " bi", scale = 1, accuracy = 0.1)) +
        labs(title = "Transferências federais para OSCs (2001-2023)",
             x = "Ano",
             y = "Valores em bilhões",
             caption = "Fonte: Siga Brasil, 2024. Nota: valores corrigos para preços de maio de 2024 pelo INPC.") +  # Nota de rodapé aqui
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.caption = element_text(hjust = 0, size = 8, face = "italic")  # Formatação da nota de rodapé
        )
#salvando o grafico com todas as transferencias federais, por ano
ggsave("figures/g_transferencias.png", plot = g_transferencias, width = 8, height = 6, dpi = 300)
