# description -------------------------------------------------------------

# Goal: helpers for NCHS measure calculations
# Author:
# Date:
# Notes:

# Load packages -----------------------------------------------------------
library(tidyverse)


# code --------------------------------------------------------------------


# subgroups for NCHS measures ---------------------------------------------

list_nchs_subgroups <- function(){
  
  df <- tribble(
    ~subgroup, ~description,
    1, 'Non-Hispanic, White alone',
    2, 'Non-Hispanic, Black alone',
    3, 'Non-Hispanic, AIAN alone',
    4, 'Non-Hispanic, Asian alone',
    5, 'Non-Hispanic, NHOPI alone',
    6, 'Non-Hispanic, TOM',
    
    11, 'Hispanic, White alone',
    12, 'Hispanic, Black',
    13, 'Hispanic, AIAN alone',
    14, 'Hispanic, Asian alone',
    15, 'Hispanic, NHOPI alone',
    16, 'Hispanic, TOM',
    
    21, 'White alone',
    22, 'Black alone',
    23, 'AIAN alone',
    24, 'Asian alone',
    25, 'NHOPI alone',
    26, 'TOM',
    
    31, 'Hispanic',
    32, 'Hispanic, non-White'
  )
  
  return(df)
}


# list of census pop estimates csv files  -------------------------------------------

list_census_pop_csv <- function(){
  df <- tribble(
    ~year, ~file_name, ~year_num,
    2022, "cc-est2022-all.csv",     4,  
    2021, "cc-est2021-all.csv",     3,  
    2020, "CC-EST2020-ALLDATA.csv", 13, 
    2019, "cc-est2019-alldata.csv", 12, 
    2018, "cc-est2018-alldata.csv", 11, 
    2017, "cc-est2017-alldata.csv", 10, 
    2016, "cc-est2016-alldata.csv", 9,  
    2015, "cc-est2015-alldata.csv", 8
  )
  
  return(df)
}


# get Census pop estimates csv file name ----------------------------------

get_census_csv_file_info <- function(year){
  csv_lst <- list_census_pop_csv()

  f_name <- csv_lst[csv_lst$year == year, ]$file_name
  yr_num <- csv_lst[csv_lst$year == year, ]$year_num

  return(
    list('year' = year, 'file_name' = f_name,
         'year_num' = yr_num)
  )

}


# read Census pop estimates csv file --------------------------------------

read_census_pop <- function(file_name, dir = census_pop_dir){
  
  print(file_name)
  
  return(
    read_csv(file.path(dir, file_name),
             na = c("X"),
             show_col_types = FALSE
    ) %>% 
      janitor::clean_names()
  )
  
}

# function to read mort data ----------------------------------------------

read_mort_data <- function(file_path, header, n_max = Inf){
  
  mort <- read_fwf(file_path,
                   n_max = n_max, guess_max = 100000,
                   col_positions = fwf_positions(
                     start = header$start,
                     end = header$end,
                     col_names = header$fld_name
                   ),
                   show_col_types = FALSE
  )
  
  return(mort)
}


# function to read natality data ------------------------------------------

read_nat_data <- function(file_path, header, n_max = Inf, trim_ws = TRUE){
  
  nata_data <- read_fwf(
    file_path,
    n_max = n_max, 
    guess_max = 100000,
    trim_ws = trim_ws,
    col_positions = fwf_positions(
      start = header$start,
      end = header$end,
      col_names = header$fld_name
    ),
    show_col_types = FALSE
  ) 
  
  return(nata_data)
}


# funct to calculate CI L, U of Poisson distribution n=1:99 --------
get_pois_CI <- function(n = 1:99) {
  
  get_cilo <- function(alpha = 0.95, n){
    alo = (1-alpha)/2
    L = qgamma(alo, n) / n
    return(L)
  }
  
  get_cihi <- function(alpha = 0.95, n){
    ahi = (1+alpha)/2
    U = qgamma(ahi, n + 1) / n
    return(U)
  }
  
  df_ci <- tibble( n = n) %>%  
    mutate(L = map_dbl(n, ~ get_cilo(alpha = 0.95, n = .x)),
           U = map_dbl(n, ~ get_cihi(alpha = 0.95, n = .x)))
  
  return(df_ci) 
}


# get mortality data file name --------------------------------------------

get_mort_file_name <- function(year){
  return(
    paste0('MULT', year, 'US.AllCnty.txt')
  )
  
}

# get natality data file name --------------------------------------------

get_nat_file_name <- function(year){
  return(
    paste0('nat', year, 'us.AllCnty.txt')
  )
  
}

# icd codes ---------------------------------------------------------------

icd_codes_v135 <- function(){
  icd_code_lst <- c('U010','U011', 'U012', 'U013', 'U014', 'U015', 'U016', 'U017', 
                    'U018', 'U019', 'U02', 'U030','U039', 'V010', 'V011', 'V019', 
                    'V020', 'V021', 'V029', 'V030', 'V031', 'V039', 'V040', 'V041', 
                    'V049', 'V050', 'V051', 'V059', 
                    'V060', 'V061', 'V069', 'V090', 'V091', 'V092', 'V093', 'V099', 'V100', 'V101', 
                    'V102', 'V103', 'V104', 'V105', 'V109', 'V110', 'V111', 'V112', 'V113', 'V114', 
                    'V115', 'V119', 'V120','V121','V122','V123','V124', 'V125', 'V129', 
                    'V130','V131','V132', 'V133','V134','V135','V139', 'V140','V141','V142', 'V143',
                    'V144','V145','V149', 'V150','V151','V152', 'V153','V154','V155','V159', 'V160',
                    'V161','V162', 'V163','V164','V165','V169',
                    'V170','V171','V172', 'V173','V174','V175','V179', 'V180','V181','V182', 'V183',
                    'V184','V185','V189', 'V190','V191','V192', 'V193','V194','V195','V196','V198',
                    'V199', 'V200','V201','V202', 'V203','V204','V205','V209',
                    'V210','V211','V212', 'V213','V214','V215','V219', 'V220','V221','V222', 'V223',
                    'V224','V225','V229', 'V230','V231','V232', 'V233','V234','V235','V239', 'V240',
                    'V241','V242', 'V243','V244','V245','V249', 
                    'V250','V251','V252', 'V253','V254','V255','V259', 'V260','V261','V262', 'V263',
                    'V264','V265','V269', 'V270','V271','V272', 'V273','V274','V275','V279', 'V280',
                    'V281','V282', 'V283','V284','V285','V289', 
                    'V290','V291','V292', 'V293','V294','V295','V296', 'V298', 'V299', 'V300', 'V301', 
                    'V302', 'V303', 'V304', 'V305', 'V306', 'V307', 'V308', 'V309', 'V310', 'V311', 
                    'V312', 'V313', 'V314', 'V315', 'V316', 'V317', 'V318', 'V319',
                    'V320', 'V321', 'V322', 'V323', 'V324', 'V325', 'V326', 'V327', 'V328', 'V329', 
                    'V330', 'V331', 'V332', 'V333', 'V334', 'V335', 'V336', 'V337', 'V338', 'V339', 
                    'V340', 'V341', 'V342', 'V343', 'V344', 'V345', 'V346', 'V347', 'V348', 'V349', 
                    'V350', 'V351', 'V352', 'V353', 'V354', 'V355', 'V356', 'V357', 'V358', 'V359', 
                    'V360', 'V361', 'V362', 'V363', 'V364', 'V365', 'V366', 'V367', 'V368', 'V369', 
                    'V370', 'V371', 'V372', 'V373', 'V374', 'V375', 'V376', 'V377', 'V378', 'V379',
                    'V380', 'V381', 'V382', 'V383', 'V384', 'V385', 'V386', 'V387', 'V388', 'V389',
                    'V390', 'V391', 'V392', 'V393', 'V394', 'V395', 'V396', 'V397', 'V398', 'V399',
                    'V400', 'V401', 'V402', 'V403', 'V404', 'V405', 'V406', 'V407', 'V408', 'V409', 
                    'V410', 'V411', 'V412', 'V413', 'V414', 'V415', 'V416', 'V417', 'V418', 'V419', 
                    'V420', 'V421', 'V422', 'V423', 'V424', 'V425', 'V426', 'V427', 'V428', 'V429', 
                    'V430', 'V431', 'V432', 'V433', 'V434', 'V435', 'V436', 'V437', 'V438', 'V439', 
                    'V440', 'V441', 'V442', 'V443', 'V444', 'V445', 'V446', 'V447', 'V448', 'V449', 
                    'V450', 'V451', 'V452', 'V453', 'V454', 'V455', 'V456', 'V457', 'V458', 'V459', 
                    'V460', 'V461', 'V462', 'V463', 'V464', 'V465', 'V466', 'V467', 'V468', 'V469', 
                    'V470', 'V471', 'V472', 'V473', 'V474', 'V475', 'V476', 'V477', 'V478', 'V479',
                    'V480', 'V481', 'V482', 'V483', 'V484', 'V485', 'V486', 'V487', 'V488', 'V489', 
                    'V490', 'V491', 'V492', 'V493', 'V494', 'V495', 'V496', 'V497', 'V498', 'V499',
                    'V500', 'V501', 'V502', 'V503', 'V504', 'V505', 'V506', 'V507', 'V508', 'V509', 
                    'V510', 'V511', 'V512', 'V513', 'V514', 'V515', 'V516', 'V517', 'V518', 'V519', 
                    'V520', 'V521', 'V522', 'V523', 'V524', 'V525', 'V526', 'V527', 'V528', 'V529', 
                    'V530', 'V531', 'V532', 'V533', 'V534', 'V535', 'V536', 'V537', 'V538', 'V539', 
                    'V540', 'V541', 'V542', 'V543', 'V544', 'V545', 'V546', 'V547', 'V548', 'V549', 
                    'V550', 'V551', 'V552', 'V553', 'V554', 'V555', 'V556', 'V557', 'V558', 'V559',
                    'V560', 'V561', 'V562', 'V563', 'V564', 'V565', 'V566', 'V567', 'V568', 'V569', 
                    'V570', 'V571', 'V572', 'V573', 'V574', 'V575', 'V576', 'V577', 'V578', 'V579', 
                    'V580', 'V581', 'V582', 'V583', 'V584', 'V585', 'V586', 'V587', 'V588', 'V589',
                    'V590', 'V591', 'V592', 'V593', 'V594', 'V595', 'V596', 'V597', 'V598', 'V599',
                    'V600', 'V601', 'V602', 'V603', 'V604', 'V605', 'V606', 'V607', 'V608', 'V609', 
                    'V610', 'V611', 'V612', 'V613', 'V614', 'V615', 'V616', 'V617', 'V618', 'V619',
                    'V620', 'V621', 'V622', 'V623', 'V624', 'V625', 'V626', 'V627', 'V628', 'V629', 
                    'V630', 'V631', 'V632', 'V633', 'V634', 'V635', 'V636', 'V637', 'V638', 'V639', 
                    'V640', 'V641', 'V642', 'V643', 'V644', 'V645', 'V646', 'V647', 'V648', 'V649', 
                    'V650', 'V651', 'V652', 'V653', 'V654', 'V655', 'V656', 'V657', 'V658', 'V659', 
                    'V660', 'V661', 'V662', 'V663', 'V664', 'V665', 'V666', 'V667', 'V668', 'V669', 
                    'V670', 'V671', 'V672', 'V673', 'V674', 'V675', 'V676', 'V677', 'V678', 'V679',
                    'V680', 'V681', 'V682', 'V683', 'V684', 'V685', 'V686', 'V687', 'V688', 'V689', 
                    'V690', 'V691', 'V692', 'V693', 'V694', 'V695', 'V696', 'V697', 'V698', 'V699',
                    'V700', 'V701', 'V702', 'V703', 'V704', 'V705', 'V706', 'V707', 'V708', 'V709',
                    'V710', 'V711', 'V712', 'V713', 'V714', 'V715', 'V716', 'V717', 'V718', 'V719', 
                    'V720', 'V721', 'V722', 'V723', 'V724', 'V725', 'V726', 'V727', 'V728', 'V729', 
                    'V730', 'V731', 'V732', 'V733', 'V734', 'V735', 'V736', 'V737', 'V738', 'V739', 
                    'V740', 'V741', 'V742', 'V743', 'V744', 'V745', 'V746', 'V747', 'V748', 'V749', 
                    'V750', 'V751', 'V752', 'V753', 'V754', 'V755', 'V756', 'V757', 'V758', 'V759', 
                    'V760', 'V761', 'V762', 'V763', 'V764', 'V765', 'V766', 'V767', 'V768', 'V769',
                    'V770', 'V771', 'V772', 'V773', 'V774', 'V775', 'V776', 'V777', 'V778', 'V779', 
                    'V780', 'V781', 'V782', 'V783', 'V784', 'V785', 'V786', 'V787', 'V788', 'V789', 
                    'V790', 'V791', 'V792', 'V793', 'V794', 'V795', 'V796', 'V797', 'V798', 'V799', 
                    'V800', 'V801', 'V802', 'V803', 'V804', 'V805', 'V806', 'V807', 'V808', 'V809', 
                    'V810', 'V811', 'V812', 'V813', 'V814', 'V815', 'V816', 'V817', 'V818', 'V819', 
                    'V820', 'V821', 'V822', 'V823', 'V824', 'V825', 'V826', 'V827', 'V828', 'V829', 
                    'V830', 'V831', 'V832', 'V833', 'V834', 'V835', 'V836', 'V837', 'V838', 'V839', 
                    'V840', 'V841', 'V842', 'V843', 'V844', 'V845', 'V846', 'V847', 'V848', 'V849', 
                    'V850', 'V851', 'V852', 'V853', 'V854', 'V855', 'V856', 'V857', 'V858', 'V859', 
                    'V860', 'V861', 'V862', 'V863', 'V864', 'V865', 'V866', 'V867', 'V868', 'V869', 
                    'V870', 'V871', 'V872', 'V873', 'V874', 'V875', 'V876', 'V877', 'V878', 'V879', 
                    'V880', 'V881', 'V882', 'V883', 'V884', 'V885', 'V886', 'V887', 'V888', 'V889', 
                    'V890', 'V891', 'V892', 'V893', 'V894', 'V895', 'V896', 'V897', 'V898', 'V899', 
                    'V900', 'V901', 'V902', 'V903', 'V904', 'V905', 'V906', 'V907', 'V908', 'V909', 
                    'V910', 'V911', 'V912', 'V913', 'V914', 'V915', 'V916', 'V917', 'V918', 'V919',
                    'V920', 'V921', 'V922', 'V923', 'V924', 'V925', 'V926', 'V927', 'V928', 'V929', 
                    'V930', 'V931', 'V932', 'V933', 'V934', 'V935', 'V936', 'V937', 'V938', 'V939', 
                    'V940', 'V941', 'V942', 'V943', 'V944', 'V945', 'V946', 'V947', 'V948', 'V949',
                    'V950', 'V951', 'V952', 'V953', 'V954', 'V955', 'V956', 'V957', 'V958', 'V959', 
                    'V960', 'V961', 'V962', 'V963', 'V964', 'V965', 'V966', 'V967', 'V968', 'V969', 
                    'V970', 'V971', 'V972', 'V973', 'V974', 'V975', 'V976', 'V977', 'V978', 'V979', 
                    'V98', 'V99', 
                    'W00', 'W01', 'W02', 'W03', 'W04', 'W05', 'W06', 'W07', 'W08', 'W09', 'W10', 
                    'W11', 'W12', 'W13', 'W14', 'W15', 'W16','W17', 'W18', 'W19',
                    'W20', 'W21', 'W22', 'W23', 'W24', 'W25', 'W26', 'W27', 'W28', 'W29', 'W30', 
                    'W31', 'W32', 'W33', 'W34', 'W35', 'W36','W37', 'W38', 'W39','W40','W41', 'W42', 
                    'W43', 'W44', 'W45', 'W46', 'W47', 'W48', 'W49', 
                    'W50', 'W51', 'W52', 'W53', 'W54', 'W55', 'W56','W57', 'W58', 'W59','W60', 'W61', 
                    'W62', 'W63', 'W64', 'W65', 'W66','W67', 'W68', 'W69','W70', 'W71', 'W72', 'W73', 'W74', 
                    'W75', 'W76','W77', 'W78', 'W79','W80', 'W81', 'W82', 'W83', 'W84', 'W85', 'W86',
                    'W87', 'W88', 'W89','W90', 'W91', 'W92', 'W93', 'W94','W99',
                    'X00', 'X01', 'X02', 'X03', 'X04', 'X05', 'X06', 'X07', 'X08', 'X09',
                    'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19','X20', 
                    'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29','X30', 'X31', 
                    'X32', 'X33', 'X340', 'X341', 'X348', 'X349', 'X35', 'X36', 'X37', 'X38', 'X39',
                    'X40', 'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49','X50', 
                    'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X590', 'X599', 'X60', 
                    'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69','X70', 'X71', 
                    'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                    'X80', 'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89','X90', 
                    'X91', 'X92', 'X93', 'X94', 'X95', 'X96', 'X97', 'X98', 'X99', 
                    'Y00', 'Y01', 'Y02', 'Y03', 'Y04', 'Y05', 'Y060', 'Y061', 'Y062', 'Y068', 'Y069', 
                    'Y070', 'Y071', 'Y072', 'Y073', 'Y078', 'Y079', 'Y08', 'Y09',
                    'Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19','Y20', 
                    'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29','Y30', 'Y31', 
                    'Y32', 'Y33', 'Y34', 'Y350', 'Y351', 'Y352', 'Y353', 'Y354', 'Y355', 'Y356', 'Y357',
                    'Y360', 'Y361', 'Y362', 'Y363', 'Y364', 'Y365', 'Y366', 'Y367', 'Y368', 
                    'Y369', 'Y850', 'Y859', 'Y86', 'Y870','Y871', 'Y872', 'Y890', 'Y891', 'Y899')
  
  return(icd_code_lst)
}

icd_codes_v039 <- function(){
  icd_code_lst <- c('V021', 'V029', 'V031', 'V039', 'V041', 'V049', 'V092', 'V123','V124', 'V125', 'V129', 
                    'V133','V134','V135','V139', 'V143','V144','V145','V149', 'V194','V195','V196','V203','V204','V205','V209',
                    'V213','V214','V215','V219', 'V223','V224','V225','V229','V233','V234','V235','V239', 'V243','V244','V245','V249', 
                    'V253','V254','V255','V259', 'V263','V264','V265','V269', 'V273','V274','V275','V279', 'V283','V284','V285','V289', 
                    'V294','V295','V296', 'V298', 'V299', 'V304', 'V305', 'V306', 'V307', 'V308', 'V309', 'V314', 'V315', 'V316', 'V317', 'V318', 'V319',
                    'V324', 'V325', 'V326', 'V327', 'V328', 'V329',  'V334', 'V335', 'V336', 'V337', 'V338', 'V339', 'V344', 'V345', 'V346', 'V347', 'V348', 'V349', 
                    'V354', 'V355', 'V356', 'V357', 'V358', 'V359',  'V364', 'V365', 'V366', 'V367', 'V368', 'V369', 'V374', 'V375', 'V376', 'V377', 'V378', 'V379',
                    'V384', 'V385', 'V386', 'V387', 'V388', 'V389', 'V394', 'V395', 'V396', 'V397', 'V398', 'V399', 'V404', 'V405', 'V406', 'V407', 'V408', 'V409', 
                    'V414', 'V415', 'V416', 'V417', 'V418', 'V419', 'V424', 'V425', 'V426', 'V427', 'V428', 'V429', 'V434', 'V435', 'V436', 'V437', 'V438', 'V439', 
                    'V444', 'V445', 'V446', 'V447', 'V448', 'V449', 'V454', 'V455', 'V456', 'V457', 'V458', 'V459', 'V464', 'V465', 'V466', 'V467', 'V468', 'V469', 
                    'V474', 'V475', 'V476', 'V477', 'V478', 'V479', 'V484', 'V485', 'V486', 'V487', 'V488', 'V489',  'V494', 'V495', 'V496', 'V497', 'V498', 'V499',
                    'V504', 'V505', 'V506', 'V507', 'V508', 'V509',  'V514', 'V515', 'V516', 'V517', 'V518', 'V519', 'V524', 'V525', 'V526', 'V527', 'V528', 'V529', 
                    'V534', 'V535', 'V536', 'V537', 'V538', 'V539',  'V544', 'V545', 'V546', 'V547', 'V548', 'V549', 'V554', 'V555', 'V556', 'V557', 'V558', 'V559',
                    'V564', 'V565', 'V566', 'V567', 'V568', 'V569',  'V574', 'V575', 'V576', 'V577', 'V578', 'V579', 'V584', 'V585', 'V586', 'V587', 'V588', 'V589',
                    'V594', 'V595', 'V596', 'V597', 'V598', 'V599','V604', 'V605', 'V606', 'V607', 'V608', 'V609', 'V614', 'V615', 'V616', 'V617', 'V618', 'V619',
                    'V624', 'V625', 'V626', 'V627', 'V628', 'V629',  'V634', 'V635', 'V636', 'V637', 'V638', 'V639', 'V644', 'V645', 'V646', 'V647', 'V648', 'V649', 
                    'V654', 'V655', 'V656', 'V657', 'V658', 'V659',  'V664', 'V665', 'V666', 'V667', 'V668', 'V669', 'V674', 'V675', 'V676', 'V677', 'V678', 'V679',
                    'V684', 'V685', 'V686', 'V687', 'V688', 'V689', 'V694', 'V695', 'V696', 'V697', 'V698', 'V699', 'V704', 'V705', 'V706', 'V707', 'V708', 'V709',
                    'V714', 'V715', 'V716', 'V717', 'V718', 'V719', 'V724', 'V725', 'V726', 'V727', 'V728', 'V729',  'V734', 'V735', 'V736', 'V737', 'V738', 'V739', 
                    'V744', 'V745', 'V746', 'V747', 'V748', 'V749',  'V754', 'V755', 'V756', 'V757', 'V758', 'V759', 'V764', 'V765', 'V766', 'V767', 'V768', 'V769',
                    'V774', 'V775', 'V776', 'V777', 'V778', 'V779',  'V784', 'V785', 'V786', 'V787', 'V788', 'V789', 'V794', 'V795', 'V796', 'V797', 'V798', 'V799', 
                    'V803', 'V804', 'V805', 'V811', 'V821',  
                    'V830', 'V831', 'V832', 'V833', 'V840', 'V841', 'V842', 'V843', 'V850', 'V851', 'V852', 'V853', 
                    'V860', 'V861', 'V862', 'V863', 'V870', 'V871', 'V872', 'V873', 'V874', 'V875', 'V876', 'V877', 'V878',  'V892')
  
  return(icd_code_lst)
  
}

# print functions ---------------------------------------------------------

print_summary_sub6662 <- function(df_sub6662, tb_title, digits = 2){
  tab_1 <- df_sub6662 %>% 
    filter(countycode != "000") %>% 
    group_by(race) %>% 
    filter(!is.infinite(rawvalue) ) %>% 
    summarise(
      count = sum(!is.na(rawvalue)),
      mean = mean(rawvalue, na.rm = TRUE),
      sd = sd(rawvalue, na.rm = TRUE),
      min = min(rawvalue, na.rm = TRUE),
      median = median(rawvalue, na.rm = TRUE),
      max = max(rawvalue, na.rm = TRUE)) %>% 
    left_join(list_nchs_subgroups(),
              by = c("race" = "subgroup")) %>% 
    relocate(description, .after = 1) %>% 
    mutate(across(c(count:max), ~if_else(is.infinite(.), NA, .))) %>% 
    gt() %>% 
    fmt_number(columns = (mean:max), decimals = digits) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightcyan"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = description,
        rows = race %in% c(1:2,4, 6, 11:12,14, 16, 23:25, 31,32) 
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightyellow"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = race,
        rows = race %in% c(1:6) 
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightgreen"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = race,
        rows = race %in% c(11:16) 
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightsalmon"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = race,
        rows = race %in% c(21:26) 
      )
    ) %>% 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>% 
    tab_header(
      title = tb_title
    )
  
  return(tab_1)
}

print_summary_sub6_1 <- function(df_sub61, tb_title, digits = 2){
  tab_1 <- df_sub61 %>% 
    filter(countycode != "000") %>% 
    group_by(race) %>% 
    filter(!is.infinite(rawvalue) ) %>% 
    summarise(
      count = sum(!is.na(rawvalue)),
      mean = mean(rawvalue, na.rm = TRUE),
      sd = sd(rawvalue, na.rm = TRUE),
      min = min(rawvalue, na.rm = TRUE),
      median = median(rawvalue, na.rm = TRUE),
      max = max(rawvalue, na.rm = TRUE)) %>% 
    mutate(description = case_when(
      race == 1 ~ "Non-Hispanic, White",
      race == 2 ~ "Non-Hispanic, Black",
      race == 3 ~ "Non-Hispanic, AIAN",
      race == 4 ~ "Non-Hispanic, Asian",
      race == 5 ~ "Non-Hispanic, NHOPI",
      race == 6 ~ "Non-Hispanic, TOM",
      race == 8 ~ "Non-Hispanic, White",
      TRUE ~ NA
    ), .after = 1) %>% 
    gt() %>% 
    fmt_number(columns = (mean:max), decimals = digits) %>% 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>% 
    tab_header(
      title = tb_title
    )
  
  return(tab_1)
}


# Mort: race recode 40 list -----------------------------------------------

list_mort_race_recode_40 <- function(){
  
  race_40 <- tibble::tribble(
    ~race_recode_40,                               ~description, ~race_recode_31,
    1L,                                    "White",              1L,
    2L,                                    "Black",              2L,
    3L, "AIAN (American Indian or Alaskan Native)",              3L,
    4L,                             "Asian Indian",              4L,
    5L,                                  "Chinese",              4L,
    6L,                                 "Filipino",              4L,
    7L,                                 "Japanese",              4L,
    8L,                                   "Korean",              4L,
    9L,                               "Vietnamese",              4L,
    10L,                  "Other or Multiple Asian",              4L,
    11L,                                 "Hawaiian",              5L,
    12L,                                "Guamanian",              5L,
    13L,                                   "Samoan",              5L,
    14L,       "Other or Multiple Pacific Islander",              5L,
    15L,                          "Black and White",              6L,
    16L,                           "Black and AIAN",              7L,
    17L,                          "Black and Asian",              8L,
    18L,                          "Black and NHOPI",              9L,
    19L,                           "AIAN and White",             10L,
    20L,                           "AIAN and Asian",             11L,
    21L,                           "AIAN and NHOPI",             12L,
    22L,                          "Asian and White",             13L,
    23L,                          "Asian and NHOPI",             14L,
    24L,                          "NHOPI and White",             15L,
    25L,                    "Black, AIAN, and White",             16L,
    26L,                    "Black, AIAN, and Asian",             17L,
    27L,                    "Black, AIAN, and NHOPI",             18L,
    28L,                   "Black, Asian, and White",             19L,
    29L,                   "Black, Asian, and NHOPI",             20L,
    30L,                   "Black, NHOPI, and White",             21L,
    31L,                    "AIAN, Asian, and White",             22L,
    32L,                    "AIAN, NHOPI, and White",             23L,
    33L,                    "AIAN, Asian, and NHOPI",             24L,
    34L,                   "Asian, NHOPI, and White",             25L,
    35L,             "Black, AIAN, Asian, and White",             26L,
    36L,             "Black, AIAN, Asian, and NHOPI",             27L,
    37L,             "Black, AIAN, NHOPI, and White",             28L,
    38L,            "Black, Asian, NHOPI, and White",             29L,
    39L,             "AIAN, Asian, NHOPI, and White",             30L,
    40L,      "Black, AIAN, Asian, NHOPI, and White",             31L,
    99L,                   "Unknown and Other Race",             99L
  )
  
  return(race_40)
  
}


# Natality: race recode 31 ------------------------------------------------

list_natl_race_recode_31 <- function(){
  race_31 <- tibble::tribble(
    ~race_recode_31,                                    ~description,
    1L,                    "White",
    2L,                                             "Black",
    3L,          "AIAN (American Indian or Alaskan Native)",
    4L,                                             "Asian",
    5L, "NHOPI (Native Hawaiian or Other Pacific Islander)",
    6L,                                          "Black and White",
    7L,                                           "Black and AIAN",
    8L,                                          "Black and Asian",
    9L,                                          "Black and NHOPI",
    10L,                                           "AIAN and White",
    11L,                                           "AIAN and Asian",
    12L,                                           "AIAN and NHOPI",
    13L,                                          "Asian and White",
    14L,                                          "Asian and NHOPI",
    15L,                                          "NHOPI and White",
    16L,                                   "Black, AIAN, and White",
    17L,                                   "Black, AIAN, and Asian",
    18L,                                   "Black, AIAN, and NHOPI",
    19L,                                  "Black, Asian, and White",
    20L,                                  "Black, Asian, and NHOPI",
    21L,                                  "Black, NHOPI, and White",
    22L,                                   "AIAN, Asian, and White",
    23L,                                   "AIAN, NHOPI, and White",
    24L,                                   "AIAN, Asian, and NHOPI",
    25L,                                  "Asian, NHOPI, and White",
    26L,                            "Black, AIAN, Asian, and White",
    27L,                            "Black, AIAN, Asian, and NHOPI",
    28L,                            "Black, AIAN, NHOPI, and White",
    29L,                           "Black, Asian, NHOPI, and White",
    30L,                            "AIAN, Asian, NHOPI, and White",
    31L,                     "Black, AIAN, Asian, NHOPI, and White",
    99L,               "Unknown"
  )
  
  return(race_31)
  
}

# end -------------------- ----------------------------------------------------


