
#Clean the company names to be all lowercase and spelled correctly
companies <- refine_original$company

replace_comp <- function(){
  p <- grep(pattern = "p", ignore.case = TRUE, companies) #p is unique char for phillips
  companies <- replace(companies, p, values = "phillips")
  z <- grep(pattern = "z", ignore.case = TRUE, companies) #z is unique char for akzo
  companies <- replace(companies, z, values = "akzo")
  t <- grep(pattern = "t", ignore.case = TRUE, companies) #t is unique char for van houten
  companies <- replace(companies, t, values = "van houten")
  r <- grep(pattern = "r", ignore.case = TRUE, companies) #r is unique char for unilever
  companies <- replace(companies, r, "unilever")
  return(companies)
}

refine_original$company <- replace_comp()

#Separate the product code and number

refine_original <- refine_original %>% separate('Product code / number', c("prod_code", "prod_num"), sep = "-")

#Add the product type column
prod_ref <- data_frame("prod_code" = c("p", "x", "v", "q"), "prod_type" = c("Smartphone", "TV", "Laptop", "Tablet"))

refine_original <- left_join(refine_original, prod_ref, by = "prod_code")

#Add full_address for geo-coding
refine_original <- unite(refine_original, "full_address", address, city, country, sep = ",", remove = FALSE)

#Add dummy variables for company and product category
refine_original <- refine_original %>% 
  mutate(company_phillips = as.integer(company == "phillips"), company_akzo = as.integer(company == "akzo"), company_van_houten = as.integer(company == "van houten"), company_unilever = as.integer(company == "unilever")) %>% 
  mutate(product_smartphone = as.integer(prod_type == "Smartphone"), product_laptop = as.integer(prod_type == "Laptop"), product_TV = as.integer(prod_type == "TV"), product_tablet = as.integer(prod_type == "Tablet"))

