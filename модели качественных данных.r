library("glmx")
library("margins")
library("numDeriv")
library("lmtest")
library("mvtnorm")

data <- read.csv('C:/Users/79670/Desktop/микрометрика/homework.csv', sep = ';')

residence <- data$residence
data$residence_1 <- as.numeric(residence == 'Capital')                  # среднее образование
data$residence_2 <- as.numeric(residence == 'City')               # среднее специальное образование
data$residence_3 <- as.numeric(residence == 'Village')               # высшее образование


# 2.1 линейная-вероятностная модель
lin_prob <- lm(formula = sub ~ age + male + series + I(age^2) + male*age,                                                 
                     data = data)
summary(lin_prob)

# 2.3
lin_prob_me <- margins(model = lin_prob,        
                                               
                     variables = NULL,         
                     type = "response")        

summary(lin_prob_me)                           

lin_sub_prob <- predict(lin_prob,                         
                    type = "response")                    

# вычисляем средний предельный эффект для age
me_const <- rep(0.003928, times = length(data))
age_mes <- (me_const - 0.0000613*data$age)
mean(age_mes)

# 3.1 пробит модель
probit_model <- glm(formula = sub ~ age + male + series + age^2 + male*age,                                                 
              data = data,                                         
              family = binomial(link = "probit"))

summary(probit_model)

# 3.3 вероятность подписки человека с определенными характеристиками
Alex <- data.frame( age = 23,
                    series = 4,
                    male = 1,
                    income = 30000)

prob_Alex <- predict( probit_model,                          
                      newdata = Alex,                        
                      type = "response")                     


# 3.4
probit_me <- margins(model = probit_model,
                     type = "response",
                     variables = NULL)

summary(probit_me)


sub_prob_probit <- predict(probit_model,                         
                    type = "response")                    

prob_sub_est_probit <- as.numeric(sub_prob_probit > 0.5)  

lin_sub_est <- as.numeric(lin_sub_prob > 0.5)  
# 3.5 доли верных предсказаний
# пробит
mean(data$sub == prob_sub_est_probit)                                
# истинная доля подписок 
sub_p <- mean(data$sub)                                  
# наивный прогноз
max(sub_p, 1 - sub_p)                                 
# линейно-вероятностная модель
mean(data$sub == lin_sub_est)                                

# 3.6
Alex <- data.frame( age = 23,
                    series = 4,
                    male = 1)


Alex_me <- margins(model = probit_model, 
                    variables = 'series',
                    at = Alex, 
                    type = "response")


summary(Alex_me)

Alex_me <- margins(model = probit_model, 
                    variables = 'age',
                    at = Alex, 
                    type = "response")


summary(Alex_me)

# 3.7
Alex_female <- Alex                                 # создаем индивида идентичного Борису,
Alex_female$male <- 0                              # но без высшего образования, а затем
prob_Alex_female <- predict(probit_model,            # считаем его вероятность дефолта
                            Alex_female, 
                             "response")
prob_Alex - prob_Alex_female                             # рассчитываем предельный эффект


# 4
probit_model <- glm(formula = sub ~ income + age + I(age^2) + male + I(income*age),                                                 
                    data = data,                                         
                    family = binomial(link = "probit"))


# 4.1
ProbitLnLExtended <- function(x,                         # вектор значений параметров
                              y,                         # зависимая переменная 
                              X,                         # матрица независимых переменных
                              is_aggregate = TRUE)       # при TRUE возвращаем логарифм
                                                         # функции правдоподобия, а при FALSE возвращаем вектор вкладов
{
  beta <- matrix(x[-c(1, 2)], ncol = 1)                  # вектор beta коэффициентов и
  t <- matrix(x[c(1, 2)], ncol = 1)                      # вектор дополнительных параметров  
                                                         # переводим в матрицу с одним столбцом
  y_li <- X %*% beta                                     # оценка линейного индекса
  y_est <- y_li + t[1] * y_li ^ 2 +                      # оценка математического ожидания 
    t[2] * y_li ^ 3                                      # латентной переменной
  
  n_obs <- nrow(X)                                       # количество наблюдений
  
  L_vec <- matrix(NA, nrow = n_obs,                      # вектор столбец вкладов наблюдений
                  ncol = 1)                              # в функцию правдоподобия
  
  is_y_0 <- (y == 0)                                     # вектор условий (y = 0)
  is_y_1 <- (y == 1)                                     # вектор условий (y = 1)
  
  L_vec[is_y_1] <- pnorm(y_est[is_y_1])                  # вклад наблюдений для которых yi = 1
  L_vec[is_y_0] <- 1 - pnorm(y_est[is_y_0])              # вклад наблюдений для которых yi = 0
  
  lnL_vec <- log(L_vec)                                  # логарифмы вкладов
  
  if(!is_aggregate)                                      # возвращаем вклады
  {                                                      # при необходимости
    return(lnL_vec)
  }
  
  lnL <- sum(lnL_vec)                                    # логарифм функции правдоподобия
  
  return(lnL)
}


# Воспользуемся созданной функцией
# Оценки модели при справедливом ограничении,
# накладываемом нулевой гипотезой
beta_est <- coef(probit_model)                           # достаем оценки из обычной пробит
beta_R <- c(0, 0, beta_est)                              # модели и приравниваем значения
names(beta_R)[c(1, 2)] <- c("t1", "t2")                  # дополнительных параметров к значениям,
# предполагаемым нулевой гипотезой
# Создадим матрицу регрессоров
X_mat <- as.matrix(model.frame(probit_model))            # достаем датафрейм с регрессорами и
X_mat[, 1] <- 1                                          # первращаем его в матрицу, а также
colnames(X_mat)[1] <- "Intercept"                        # заменяем зависимую переменную на константу
head(X_mat, 5)
                                                         # Применим функцию
lnL_R <- ProbitLnLExtended(beta_R, data$sub, X_mat)      # считаем логарифм функции правоподобия
# при ограничениях, совпадающую с логарифмом
# функции правдоподобия обычной пробит модели
lnL_R_grad <- grad(func = ProbitLnLExtended,             # считаем градиент данной функции
                   x = beta_R,                           # численным методом
                   y = data$sub, 
                   X = X_mat)
lnL_R_grad <- matrix(lnL_R_grad, ncol = 1)               # градиент как матрица с одним столбцом
lnL_R_Jac <- jacobian(func = ProbitLnLExtended,          # считаем Якобин данной функции
                      x = beta_R,                        # численным методом
                      y = data$sub, 
                      X = X_mat,
                      is_aggregate = FALSE)
as_cov_est <- solve(t(lnL_R_Jac) %*% lnL_R_Jac)          # cчитаем оценку асимптотической ковариационной
                                                         # матрицы с помощью Якобианов, поскольку 
                                                         # численным методом Гессиан считается
                                                         # в данном случае очень плохо
                                                         # Реализуем тест
LM_value_1 <- t(lnL_R_grad) %*%                          # считаем статистику теста
  as_cov_est %*%                                         # множителей Лагранжа
  lnL_R_grad
p_value_1 <- 1 - pchisq(LM_value_1, df = 2)              # рассчитываем p-value теста
                                                         # множителей Лагранжа


# 4.2
hetprobit_model <- hetglm(formula = sub ~ age + male + series # основного уравнения
                          + age^2 + male*age|                 # линейный индекс   
                            age + series,                     # уравнения дисперсии
                            data = data,                                 
                            family = binomial(link = "probit"))

# Осуществим тест на гомоскедастичность:
# H0: tau = 0
lrtest(hetprobit_model, probit_model)                             

# 4.3

summary(hetprobit_model)

hetprobit_me <- margins(model = hetprobit_model,
                        variables = colnames(hetprobit_model),
                        type = "response")



  # Считаем предельный эффект с помощью
  # численного дифференцирования
delta <- 1e-6                                                    # приращение                                                 
Alex_delta <- Alex
Alex_delta$age <- Alex$age + delta

prob_Alex <- predict(hetprobit_model, newdata = Alex,          # оценка вероятности 
                      type = "response")

prob_Alex_delta <- predict(hetprobit_model, 
                            newdata = Alex_delta,
                            type = "response")
ME_age_2 <- (prob_Alex_delta - prob_Alex) / delta





# Достанем полученные оценки
beta_est <- model_hetprobit$coefficients$mean                    # оценки коэффициентов при переменных
# основного уравнения
tau_est <- model_hetprobit$coefficients$scale                    # оценки коэффициентов при переменных
# в уравнении дисперсии

# Достанем оценки стандартных отклонений
# случайных ошибок
sigma_est <- predict(model_hetprobit, type = "scale")
head(sigma_est, 10)

# 4.4 
# 1)
probit_model_age <- glm(formula = sub ~ age + male + series + age^2 + male*age,                                                 
                    data = data,                                         
                    family = binomial(link = "probit"))

probit_model_no_linage <- glm(formula = sub ~male + series + age^2 + male*age,                                                 
                    data = data,                                         
                    family = binomial(link = "probit"))

lrtest(probit_model_age, probit_model_no_linage) 

# 2)

probit_model_age <- glm(formula = sub ~ age + male + series + age^2 + male*age,                                                 
                        data = data,                                         
                        family = binomial(link = "probit"))

probit_model_no_age <- glm(formula = sub ~ male + series + male*age,                                                 
                           data = data,                                         
                           family = binomial(link = "probit"))

lrtest(probit_model_age, probit_model_no_age) 

probit_model_432 <- glm(formula = sub ~ age + male + series + age^2 + male*age, 
                       data = data,
                       family = binomial(link = "probit")) 

summary(probit_model_432)

# 3)
model_probit <- glm(formula = sub ~ age + male + series + I(age^2) + male*age, 
                       data = data,
                       family = binomial(link = "probit")) 

model_probit_R <- glm(formula = sub ~ age +  male + series + I(age^2) + male*age, 
                      data = data,
                      offset = 777*I(age^2),
                      family = binomial(link = "probit")) 

lrtest(model_probit, model_probit_R)

# 4) - проверка гипотезы H0: beta_2/beta_3=2, beta_4=0.01
model_probit <- glm(formula = sub ~ age + I(age^2) + male + series + male*age,
                    data = data,
                    family = binomial(link = "probit")) 
model_probit_R <- glm(formula = sub ~ I(age + 777*I(age^2)) + male + series + male*age, 
                      data = data,
                      offset = I(-1825*male),
                      family = binomial(link = "probit")) 
lrtest(model_probit, model_probit_R)


# 4.5
probit_model_sex_sep <- glm(formula = sub ~ age + male + series + age^2 + male*age,                                                 
                           data = data,                                         
                           family = binomial(link = "probit"))

probit_model_sex_combined <- glm(formula = sub ~ age + series + age^2,                                                 
                        data = data,                                         
                        family = binomial(link = "probit"))


lrtest(probit_model_sex_sep, probit_model_sex_combined) 

# 4.6
probit_model_capital <- glm(formula = sub ~ age + male + series + age^2 + male*age + residence_1,                                                 
                        data = data,                                         
                        family = binomial(link = "probit"))


probit_model_comb <- glm(formula = sub ~ age + male + series + age^2 + male*age,                                                 
                           data = data,                                         
                           family = binomial(link = "probit"))

lrtest(probit_model_capital, probit_model_comb)

probit_model_city <- glm(formula = sub ~ age + male + series + age^2 + male*age + residence_2,                                                 
                        data = data,                                         
                        family = binomial(link = "probit"))

lrtest(probit_model_city, probit_model_comb)

probit_model_village <- glm(formula = sub ~ age + male + series + age^2 + male*age + residence_3,                                                 
                        data = data,                                         
                        family = binomial(link = "probit"))

lrtest(probit_model_village, probit_model_comb)


# 5.1

logit_model <- glm(formula = sub ~ age + male + series + I(age^2) + male*age,                                                 
                    data = data,                                         
                    family = binomial(link = "logit"))

summary(logit_model)

sub_prob_logit <- predict(logit_model,                         
                           type = "response")                    

prob_sub_est_logit <- as.numeric(sub_prob_logit > 0.5)  


coef_est <- logit_model$coefficients


#5.2
# Отношение шансов - (вероятность успеха) / (вероятность неудачи)

# Оценим, во сколько раз, при прочих равных,
# изменится отношение шансов при
OR_series <- exp(coef_est["series"]) # изменении количества просмотренных за год сериалов на единицу
OR_male <- exp(coef_est["male"])        


# 5.3
OR_age <- exp(coef_est["age"] +             # изменении возраста на единицу
                coef_est["I(age^2)"] +
                2 * coef_est["I(age^2)"] * 
                logit_model$data$age + 
                coef_est["male:age"])

#head(OR_age, 10)


Alex_me <- margins(model = logit_model, 
                    at = Alex, 
                    type = "response")
# По непрерывным переменным
summary(Alex_me)                                     # предельный эффект высшего образования

# 5.4 --------


# 6.1
library("GJRM")
library("pbivnorm")

sub_formula <- sub ~ age + male + series + I(age^2) + male*age
TV_formula <- TV ~ age + marriage + log(income) + I(age^2)

model_bp <- gjrm(formula = list(sub_formula,         
                                TV_formula),        
                 data = data,
                 Model = "B",                           
                 margins = c("probit", "probit"),       
                 BivD = "N")                            

summary(model_bp)


# 6.3
rho_est <- model_bp$theta
options(scipen = 3)
cov_est <- solve(model_bp$fit$hessian)
std_rho <- sqrt(cov_est["theta.star", "theta.star"])
p_value_rho <- 2 * min(pnorm(rho_est / std_rho),
                       1- pnorm(rho_est / std_rho))     

# 6.4

Alex <- data.frame( age = 23,
                    series = 4,
                    male = 1,
                    marriage = 0,
                    income = 30000,
                    TV = 0)

# Получим оценки линейных индексо

sub_li <- predict(model_bp, eq = 1, newdata=Alex)
tv_li <- predict(model_bp, eq = 2, newdata=Alex)

# Оценим для индивидов вероятность:
p_sub <- predict(model_bp,
               type = "response",
               eq = 1,
               newdata = Alex) 

p_tv <- predict(model_bp,
               type = "response",
               eq = 2,
               newdata = Alex)

p_both <- pbivnorm(x = matrix(c(sub_li, tv_li),
                              ncol=2),
                   rho=rho_est)

p_sub_rare_tv_watсher <- p_both/(1-p_tv)

# 6.5
# Допустим, Алекс смотрит телевизор не реже раза в неделю 
Alex_new <- Alex
Alex_new$TV <- 1

sub_li_new <- predict(model_bp2, eq = 1,          # пересчитаем оценку линейного индекса
                          newdata = Alex_new)  
p_1_n2 <- pbivnorm(x = matrix(c(default_li_new,       # вероятность притерпеть дефолт и не
                                -stable_li),          # иметь стабильной работы
                              ncol = 2),               
                   rho = -rho_est)
p_1_cond_n2 <- p_1_n2 / (1 - p_2)                     # оценка искомой условной вероятности

# Оценим предельный эффект стабильной работы
# на вероятность дефолта
ME_stable <- p_1_cond_2 - p_1_cond_n2


# 7.1

# истинная доля подписок 
sub_p <- mean(data$sub)                                  

sub_prob_syst <- predict(model_bp,                         
                         type = "response",
                         eq = 1)                    

prob_sub_est_syst <- as.numeric(sub_prob_syst > 0.5)  

# доли верных предсказаний
# наивный прогноз
max(sub_p, 1 - sub_p)                                 
# пробит
mean(data$sub == prob_sub_est_probit)                                
# линейно-вероятностная модель
mean(data$sub == lin_sub_est)                                
# логит
mean(data$sub == prob_sub_est_logit)                                
# линейно-вероятностная модель
mean(data$sub == lin_sub_est)                                
# системы бинарных уравнений
mean(data$sub == prob_sub_est_syst)

# 7.2

AIC(probit_model)
AIC(logit_model)
AIC(lin_prob)
AIC(model_bp)

# 7.3

model_bp_1.1 <- gjrm(formula = list(sub_formula,         
                                TV_formula),        
                 data = data,
                 Model = "B",                           
                 margins = c("probit", "probit"),       
                 BivD = "C0",
                 subset = c(1:4500))                            

model_bp_2.1 <- gjrm(formula = list(sub_formula,         
                                  TV_formula),        
                   data = data,
                   Model = "B",                           
                   margins = c("probit", "probit"),       
                   BivD = "C90",
                   subset = c(1:4500))                            

model_bp_3.1 <- gjrm(formula = list(sub_formula,         
                                  TV_formula),        
                   data = data,
                   Model = "B",                           
                   margins = c("probit", "probit"),       
                   BivD = "C180",
                   subset = c(1:4500))

model_bp_4.1 <- gjrm(formula = list(sub_formula,         
                                  TV_formula),        
                   data = data,
                   Model = "B",                           
                   margins = c("probit", "probit"),       
                   BivD = "C270",
                   subset = c(1:4500))

model_bp_5.1 <- gjrm(formula = list(sub_formula,         
                                  TV_formula),        
                   data = data,
                   Model = "B",                           
                   margins = c("probit", "probit"),       
                   BivD = "GAL0",
                   subset = c(1:4500))

model_bp_1.2 <- gjrm(formula = list(sub_formula,         
                                    TV_formula),        
                     data = data,
                     Model = "B",                           
                     margins = c("logit", "logit"),       
                     BivD = "C0",
                     subset = c(1:4500))                            

model_bp_2.2 <- gjrm(formula = list(sub_formula,         
                                    TV_formula),        
                     data = data,
                     Model = "B",                           
                     margins = c("logit", "logit"),       
                     BivD = "C90",
                     subset = c(1:4500))                            

model_bp_3.2 <- gjrm(formula = list(sub_formula,         
                                    TV_formula),        
                     data = data,
                     Model = "B",                           
                     margins = c("logit", "logit"),       
                     BivD = "C180",
                     subset = c(1:4500))

model_bp_4.2 <- gjrm(formula = list(sub_formula,         
                                    TV_formula),        
                     data = data,
                     Model = "B",                           
                     margins = c("logit", "logit"),       
                     BivD = "C270",
                     subset = c(1:4500))

model_bp_5.2 <- gjrm(formula = list(sub_formula,         
                                    TV_formula),        
                     data = data,
                     Model = "B",                           
                     margins = c("logit", "logit"),       
                     BivD = "GAL0",
                     subset = c(1:4500))

AIC(model_bp_1.1)
AIC(model_bp_2.1)
AIC(model_bp_3.1)
AIC(model_bp_4.1)
AIC(model_bp_5.1)
AIC(model_bp_1.2)
AIC(model_bp_2.2)
AIC(model_bp_3.2)
AIC(model_bp_4.2)
AIC(model_bp_5.2)

library('Metrics')

mean(data$sub[1:4500])

sub_prob_1.1 <- predict(model_bp_1.1,                         
                         type = "response",
                         eq = 1)                    

prob_sub_est_1.1 <- as.numeric(sub_prob_1.1 > 0.5)  

sub_prob_2.1 <- predict(model_bp_2.1,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_2.1 <- as.numeric(sub_prob_2.1 > 0.5)  

sub_prob_3.1 <- predict(model_bp_3.1,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_3.1 <- as.numeric(sub_prob_3.1 > 0.5)  

sub_prob_4.1 <- predict(model_bp_4.1,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_4.1 <- as.numeric(sub_prob_4.1 > 0.5)  

sub_prob_5.1 <- predict(model_bp_5.1,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_5.1 <- as.numeric(sub_prob_5.1 > 0.5)  

sub_prob_1.2 <- predict(model_bp_1.2,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_1.2 <- as.numeric(sub_prob_1.2 > 0.5)  

sub_prob_2.2 <- predict(model_bp_2.2,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_2.2 <- as.numeric(sub_prob_2.2 > 0.5)  

sub_prob_3.2 <- predict(model_bp_3.2,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_3.2 <- as.numeric(sub_prob_3.2 > 0.5)  

sub_prob_4.2 <- predict(model_bp_4.2,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_4.2 <- as.numeric(sub_prob_4.2 > 0.5)  

sub_prob_5.2 <- predict(model_bp_5.2,                         
                        type = "response",
                        eq = 1)                    

prob_sub_est_5.2 <- as.numeric(sub_prob_5.2 > 0.5)  

mean(data$sub[1:4500] == prob_sub_est_1.1)
mean(data$sub[1:4500] == prob_sub_est_2.1)
mean(data$sub[1:4500] == prob_sub_est_3.1)
mean(data$sub[1:4500] == prob_sub_est_4.1)
mean(data$sub[1:4500] == prob_sub_est_5.1)
mean(data$sub[1:4500] == prob_sub_est_1.2)
mean(data$sub[1:4500] == prob_sub_est_2.2)
mean(data$sub[1:4500] == prob_sub_est_3.2)
mean(data$sub[1:4500] == prob_sub_est_4.2)
mean(data$sub[1:4500] == prob_sub_est_5.2)

# 8.2
set.seed(123)
n <- 5000
data_2 <- data.frame(income = exp(rnorm(n, 10, 0.7)))
data_2$age = round(runif(n, 20, 100))
educ = t(rmultinom(n, 1, c(0.5, 0.3, 0.2)))
data_2$educ_3 <- as.numeric(educ[, 3] == 1)
eps <- rt(n, df = 5)

beta <- c(5, 0.9, 0.04, -0.01)
          

invest_li <- beta[1] + 
  beta[2] * log(data_2$income) +              # линейный индекс,
  beta[3] * data_2$age +                      # отражающий вклад наблюдаемых
  beta[4] * data_2$educ_3
  
invest_star <- invest_li  + eps                    # латентная переменная,

data_2$invest <- as.numeric(invest_star >= 0)

lnL <- function(par, y, X)
{
  beta <- par
  n <- length(y)
  y_li <- X%*%beta
  L <- rep(NA, n)
  L[y==1] <- pt(y_li[y==1], 5)
  L[y==0] <- 1 - pt(y_li[y==0], 5)
  return(sum(log(L)))

}

lnL(c(1, 0.0001, 0.0001, 0.0001), y = data_2$invest, X = cbind(1, log(data_2$income), data_2$age, data_2$educ_3))
x0 <- c(1, 0.2, 0.00001, 0.0001)

opt <- optim(par = x0,
             fn = lnL,
             method = "BFGS",
             control = list(maxit = 100000,
                            reltol = 1e-10,
                            abstol = 1e-10,
                            fnscale = -1),
             y = data_2$invest,
             X = cbind(1, log(data_2$income), data_2$age, data_2$educ_3))

opt$par
