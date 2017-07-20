
basic <- read.csv("qog_bas_cs_jan17_new.csv", header=T, sep = ",")
prisoners <- read.csv("prison_rate_per_country_new.csv", header = T, sep = ";")
homocide <-read.csv("homocide_rate_per_country_new.csv", header=T, sep=";", dec = ",")
names(basic)[names(basic) == "cname"] <- "country"
#diff1<-setdiff(homocide$country, prisoners$country)
#diff2<-setdiff(crime$country,data$country)
crime <- merge(homocide, prisoners, by="country")
data <- merge(crime, basic, by="country")

features <- c('Subregion', 
              'homocides_rate_per_100000' , 'prisoners_rate_per_100000' ,"arda_angenpct", 
              'fh_aor' ,'fh_cl' ,'fh_ep' ,#"wef_dtsb",
              'fh_feb' ,'fh_fotpa' ,'fh_fotpb', 'fh_fotpc' ,'fh_fotpsc', 'fh_fotpst', 'fh_ipolity2' ,
'fh_pair', 'fh_pr', 'fh_rol', 'fh_status','wbgi_rle'#'wbgi_pse' ,
             )
regression_data <- na.omit(data[features])

nrow(regression_data)

poisson_regression_data <- regression_data[-which(colnames(regression_data)=="fh_fotpsc")]
poisson_regression_data["homocides_per_10e7"] <- (poisson_regression_data["homocides_rate_per_100000"]*100)
poisson_regression_data <- poisson_regression_data[-which(colnames(regression_data)=="homocides_rate_per_100000")]
#poisson_regression_data <- poisson_regression_data[-which(data$country == "El Salvador"),]

model <- glm(
    homocides_per_10e7 ~ .,
    family=poisson,
    data=poisson_regression_data
)

summary(model)

start_model <- glm(
    homocides_per_10e7 ~ .,
    family=poisson,
    data=poisson_regression_data
)
final_model <- step(start_model, direction="both", scope=(homocides_per_10e7 ~ 1))

summary(final_model)

plot(exp(predict(final_model)), poisson_regression_data$homocides_per_10e7, xlab = "predicted homocide rate", ylab = "true homocide rate", pch=3, col="blue")

rsq <- cor(exp(predict(final_model)), poisson_regression_data$homocides_per_10e7)

sqrt(rsq)
