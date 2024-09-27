###
# instalação do pacote RPostgres
library("RPostgres")
library(DBI)
# conectar ao banco de dados
con <- dbConnect(RPostgres::Postgres(),dbname = 'portal_osc2', 
                 host = 'psql12', 
                 port = 5432,
                 user = 'r1705296',
                 password = '')
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



#######################################
##AGORA OS DEZ MAIORES VALORES, POR ANO
#######################################


# realizar consulta
res <- dbGetQuery(con, "WITH summed_values AS (
    SELECT nr_orcamento_ano, 
           nr_orcamento_cnpj, 
           SUM(nr_vl_empenhado_def_inpc_junho_24) AS total_empenhado
    FROM public.tb_orcamento_def_v3
    GROUP BY nr_orcamento_ano, nr_orcamento_cnpj
),
ranked_values AS (
    SELECT nr_orcamento_ano, 
           nr_orcamento_cnpj, 
           total_empenhado,
           ROW_NUMBER() OVER (PARTITION BY nr_orcamento_ano ORDER BY total_empenhado DESC) AS rn
    FROM summed_values
)
SELECT nr_orcamento_ano, 
       nr_orcamento_cnpj, 
       total_empenhado
FROM ranked_values
WHERE rn <= 10
ORDER BY nr_orcamento_ano, total_empenhado DESC;")

# finalizar conexão com o servidor (banco de dados)
dbDisconnect(con)

# para salvar o resultado da consulta em formato .csv
# a função é write.csv2(tabela, "nomedatabela.csv", fileEncoding = "UTF-8").  # Lembrar de definir a pasta de trabalho (ctrl+shift+h) antes. É nela que a   # tabela será salva.A função salva automaticamente com o ";" como separador.  # Exemplo:
write.csv2(res,"data/top_transferencia_osc.csv", fileEncoding = "UTF-8")

top_trans <-  res 

#renomeando a variavel
top_trans <-  top_trans %>%
        rename(ano = nr_orcamento_ano,
               CNPJ = nr_orcamento_cnpj)


top_trans  <- top_trans %>% 
        mutate(total_empenhado_milhoes = round(total_empenhado/1000000))

               

#grafico para reportar os maiores valores por ano
# Converter CNPJ para character
top_trans$CNPJ <- as.character(top_trans$CNPJ)

top_trans_filtered <- top_trans %>%
        filter(ano != c(2001, 2021)) %>% 
        group_by(ano) %>%
        top_n(5, wt = total_empenhado_milhoes)


ggplot(top_trans_filtered, aes(x = CNPJ, y = total_empenhado_milhoes)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        facet_wrap(~ ano, scales = "free_y" ) +
        labs(
                title = "Total Empenhado (Milhões) por CNPJ por Ano",
                x = "CNPJ",
                y = "Total Empenhado (Milhões)"
        ) +
        theme_bw() +
        theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                strip.text = element_text(size = 12, face = "bold")
        )


########################################3
##CALCULANDO NUMERO DE OSCS, MEDIAS E MEDIANAS DAS PARCERIAS FEDERAIS
#####################################

transf_media <- dbGetQuery(con,"WITH total_transfer_per_cnpj AS (
    SELECT 
        nr_orcamento_ano,
        nr_orcamento_cnpj,
        SUM(nr_vl_empenhado_def) AS total_transfer
    FROM 
        public.tb_orcamento_def_v3
    where nr_orcamento_cnpj != 28719664000124
    GROUP BY 
        nr_orcamento_ano, 
        nr_orcamento_cnpj
)
SELECT 
    nr_orcamento_ano,
    COUNT(nr_orcamento_cnpj) AS total_cnpjs,
    ROUND(AVG(total_transfer)) AS valor_medio_transferencias,
    ROUND(PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY total_transfer)) AS valor_mediano_transferencias
FROM 
    total_transfer_per_cnpj
GROUP BY 
    nr_orcamento_ano
ORDER BY 
    nr_orcamento_ano ASC;")

# finalizar conexão com o servidor (banco de dados)
dbDisconnect(con)

write.csv2(transf_media, "data/transf_media.csv",  fileEncoding = "UTF-8")

head(transf_media)

transf_media <- transf_media %>%
        rename(ano = nr_orcamento_ano
               )

##total de OSCs que receberam recursos por ano
g_num_osc <- ggplot(data = transf_media, aes(x = ano, y = total_cnpjs)) +
                            geom_line() +
                            geom_point() +
labs(
        title = "Total de OSCs que receberam recursos federais, por ano (2001-2023)",
        x = "Ano",
        y = "Total de OSCs (i.e. CNPJs únicos)") +
        theme_classic()

##valor médio das transferencias federais, por CNPJ
g_transf_media <- ggplot(data = transf_media, aes(x = ano, y = valor_medio_transferencias)) +
        geom_line() +
        geom_point() +
        labs(
                title = "Valor médio dos valores transferidos por CNPJ, por ano (2001-2023)",
                x = "Ano",
                y = "Valor Médio Transferido (em R$)"
        ) +
        theme_classic() +
        scale_y_continuous(labels = scales::comma)  # Remover notação científica e adicionar separador de milhar


###Valores medianos transferidos, por CNPJ
##valor médio das transferencias federais, por CNPJ
g_transf_mediana <- ggplot(data = transf_media, aes(x = ano, y = valor_mediano_transferencias)) +
        geom_line() +
        geom_point() +
        labs(
                title = "Valor mediano dos valores transferidos por CNPJ, por ano (2001-2023)",
                x = "Ano",
                y = "Valor Mediano Transferido (em R$)"
        ) +
        theme_classic() +
        scale_y_continuous(labels = scales::comma)  # Remover notação científica e adicionar separador de milhar
