with got_to_collections AS (
    SELECT 
        loan_id,
        to_eat(entered_collections_at) entered_collections_at, 
        to_eat(due_date) due_date,
        to_eat(written_off_at) written_off_at,
        extract(day from (now() - due_date)) days_in_arrears,
        to_eat(completed_at) completed_at,
        business_id,
        business_name,
        loan_name
    FROM rich_loan_view 
    WHERE entered_collections_at notnull
    ORDER BY payment_status
),loans_in_collections AS (
    SELECT 
       loan_id,
       officer
    FROM (values
    (72624,'Maggie'),	(74013,'Maggie'),	(74668,'Maggie'),	(75363,'Maggie'),	(75614,'Maggie'),	(75690,'Maggie'),	(75732,'Maggie'),	(75797,'Josephine'),	(75897,'Josephine'),	(75913,'Josephine'),	(76029,'Maggie'),	(76068,'Maggie'),	(76107,'Josephine'),	(76201,'Josephine'),	(76260,'Josephine'),	(76270,'Josephine'),	(76282,'Josephine'),	(76310,'Josephine'),	(76363,'Josephine'),	(76477,'Maggie'),	(76521,'Maggie'),	(76574,'Maggie'),	(76625,'Maggie'),	(76642,'Josephine'),	(76829,'Josephine'),	(76891,'Josephine'),	(76917,'Maggie'),	(77175,'Maggie'),	(77186,'Josephine'),	(77225,'Josephine'),	(77275,'Maggie'),	(77340,'Maggie'),	(77354,'Maggie'),	(77466,'Josephine'),	(77471,'Maggie'),	(77492,'Maggie'),	(77530,'Josephine'),	(77547,'Maggie'),	(77556,'Josephine'),	(77607,'Maggie'),	(77622,'Josephine'),	(77792,'Maggie'),	(77810,'Josephine'),	(77921,'Maggie'),	(78089,'Josephine'),	(78107,'Maggie'),	(78126,'Maggie'),	(78139,'Maggie'),	(78146,'Josephine'),	(78196,'Maggie'),	(78214,'Josephine'),	(78356,'Maggie'),	(78567,'Maggie'),	(78621,'Josephine'),	(78703,'Josephine'),	(78728,'Maggie'),	(78864,'Maggie'),	(78874,'Josephine'),	(78881,'Josephine'),	(78885,'Josephine'),	(78967,'Josephine'),	(78992,'Maggie'),	(79014,'Maggie'),	(79031,'Josephine'),	(79060,'Josephine'),	(79177,'Josephine'),	(79187,'Maggie'),	(79216,'Josephine'),	(79284,'Maggie'),	(79305,'Maggie'),	(79338,'Josephine'),	(79346,'Josephine'),	(79373,'Maggie'),	(79466,'Maggie'),	(79478,'Maggie'),	(79490,'Maggie'),	(79623,'Maggie'),	(79668,'Maggie'),	(79717,'Maggie'),	(79925,'Josephine'),	(79958,'Maggie'),	(79999,'Maggie'),	(80011,'Josephine'),	(80051,'Josephine'),	(80055,'Maggie'),	(80101,'Maggie'),	(80191,'Josephine'),	(80256,'Josephine'),	(80286,'Josephine'),	(80389,'Josephine'),	(80523,'Josephine'),	(80577,'Josephine'),	(80650,'Josephine'),	(80675,'Josephine'),	(80690,'Josephine'),	(80777,'Maggie'),	(80814,'Josephine'),	(80898,'Maggie'),	(80997,'Josephine'),	(81083,'Josephine'),	(81130,'Maggie'),	(81144,'Maggie'),	(81179,'Maggie'),	(81228,'Maggie'),	(81273,'Josephine'),	(81309,'Maggie'),	(81314,'Josephine'),	(81319,'Maggie'),	(81322,'Maggie'),	(81343,'Maggie'),	(81351,'Maggie'),	(81421,'Maggie'),	(81442,'Maggie'),	(81448,'Josephine'),	(81454,'Maggie'),	(81484,'Josephine'),	(81491,'Maggie'),	(81554,'Josephine'),	(81611,'Josephine'),	(81735,'Josephine'),	(81788,'Maggie'),	(81795,'Josephine'),	(81856,'Josephine'),	(81857,'Maggie'),	(81880,'Josephine'),	(81968,'Maggie'),	(81982,'Josephine'),	(82010,'Josephine'),	(82101,'Maggie'),	(82127,'Josephine'),	(82133,'Maggie'),	(82180,'Maggie'),	(82209,'Josephine'),	(82271,'Maggie'),	(82294,'Josephine'),	(82299,'Josephine'),	(82324,'Josephine'),	(82337,'Josephine'),	(82338,'Josephine'),	(82371,'Josephine'),	(82423,'Josephine'),	(82516,'Maggie'),	(82543,'Maggie'),	(82594,'Maggie'),	(82697,'Maggie'),	(82755,'Maggie'),	(82810,'Maggie'),	(82885,'Maggie'),	(82909,'Maggie'),	(82919,'Josephine'),	(82963,'Maggie'),	(82977,'Josephine'),	(83000,'Maggie'),	(83189,'Josephine'),	(83197,'Josephine'),	(83209,'Maggie'),	(83217,'Josephine'),	(83225,'Josephine'),	(83262,'Josephine'),	(83268,'Josephine'),	(83308,'Josephine'),	(83326,'Josephine'),	(83333,'Josephine'),	(83360,'Josephine'),	(83363,'Josephine'),	(83365,'Josephine'),	(83371,'Josephine'),	(83511,'Maggie'),	(83537,'Maggie'),	(83610,'Josephine'),	(83615,'Maggie'),	(83707,'Josephine'),	(83782,'Josephine'),	(83871,'Josephine'),	(83872,'Maggie'),	(83896,'Maggie'),	(83908,'Josephine'),	(83963,'Maggie'),	(84023,'Maggie'),	(84083,'Josephine'),	(84340,'Maggie'),	(84342,'Josephine'),	(84379,'Maggie'),	(84449,'Maggie'),	(84521,'Maggie'),	(84527,'Josephine'),	(84576,'Josephine'),	(84590,'Josephine'),	(84642,'Maggie'),	(84647,'Maggie'),	(84660,'Maggie'),	(84695,'Maggie'),	(84806,'Maggie'),	(84868,'Josephine'),	(84884,'Maggie'),	(84898,'Maggie'),	(84939,'Maggie'),	(85020,'Maggie'),	(85029,'Maggie'),	(85038,'Josephine'),	(85045,'Josephine'),	(85135,'Josephine'),	(85218,'Maggie'),	(85385,'Josephine'),	(85403,'Maggie'),	(85488,'Josephine'),	(85489,'Josephine'),	(85586,'Josephine'),	(85597,'Josephine'),	(85646,'Maggie'),	(85654,'Josephine'),	(85665,'Maggie'),	(85735,'Josephine'),	(85819,'Maggie'),	(85836,'Maggie'),	(85874,'Josephine'),	(85892,'Maggie'),	(85896,'Josephine'),	(85900,'Josephine'),	(86024,'Josephine'),	(86025,'Maggie'),	(86122,'Maggie'),	(86164,'Maggie'),	(86232,'Maggie'),	(86269,'Josephine'),	(86329,'Maggie'),	(86333,'Maggie'),	(86344,'Maggie'),	(86370,'Josephine'),	(86376,'Josephine'),	(86379,'Maggie'),	(86383,'Josephine'),	(86520,'Josephine'),	(86524,'Maggie'),	(86552,'Josephine'),	(86592,'Maggie'),	(86738,'Josephine'),	(86756,'Josephine'),	(86811,'Maggie'),	(86986,'Josephine'),	(86987,'Maggie'),	(87055,'Josephine'),	(87068,'Josephine'),	(87069,'Josephine'),	(87070,'Josephine'),	(87104,'Josephine'),	(87144,'Josephine'),	(87232,'Maggie'),	(87351,'Josephine'),	(87373,'Josephine'),	(87426,'Maggie'),	(87502,'Maggie'),	(87520,'Josephine'),	(87530,'Maggie'),	(87599,'Josephine'),	(87620,'Maggie'),	(87694,'Josephine'),	(87713,'Josephine'),	(87715,'Josephine'),	(87800,'Josephine'),	(87874,'Josephine'),	(87906,'Josephine'),	(87907,'Maggie'),	(87996,'Josephine'),	(88008,'Maggie'),	(88066,'Josephine'),	(88077,'Josephine'),	(88127,'Maggie'),	(88171,'Josephine'),	(88197,'Josephine'),	(88277,'Maggie'),	(88287,'Maggie'),	(88309,'Josephine'),	(88310,'Maggie'),	(88392,'Josephine'),	(88395,'Maggie'),	(88404,'Maggie'),	(88419,'Josephine'),	(88452,'Josephine'),	(88569,'Josephine'),	(88652,'Josephine'),	(88683,'Josephine'),	(88692,'Josephine'),	(88697,'Josephine'),	(88719,'Josephine'),	(88732,'Josephine'),	(88739,'Josephine'),	(88747,'Josephine'),	(88773,'Maggie'),	(88778,'Maggie'),	(88833,'Maggie'),	(88842,'Josephine'),	(88989,'Josephine'),	(89159,'Maggie'),	(89198,'Maggie'),	(89227,'Josephine'),	(89286,'Josephine'),	(89312,'Josephine'),	(89332,'Maggie'),	(89375,'Josephine'),	(89415,'Maggie'),	(89480,'Josephine'),	(89642,'Josephine'),	(89672,'Josephine'),	(89724,'Maggie'),	(89758,'Josephine'),	(89909,'Josephine'),	(89936,'Maggie'),	(89961,'Josephine'),	(89966,'Josephine'),	(90020,'Maggie'),	(90148,'Josephine'),	(90179,'Josephine'),	(90197,'Maggie'),	(90221,'Josephine'),	(90314,'Maggie'),	(90371,'Josephine'),	(90389,'Josephine'),	(90428,'Josephine'),	(90459,'Josephine'),	(90479,'Josephine'),	(90496,'Josephine'),	(90530,'Josephine'),	(90570,'Maggie'),	(90601,'Josephine'),	(90629,'Maggie'),	(90744,'Josephine'),	(90780,'Maggie'),	(90799,'Josephine'),	(90816,'Maggie'),	(90907,'Josephine'),	(90912,'Josephine'),	(90923,'Josephine'),	(90926,'Josephine'),	(90931,'Maggie'),	(90981,'Josephine'),	(91029,'Josephine'),	(91043,'Maggie'),	(91066,'Josephine'),	(91082,'Josephine'),	(91148,'Josephine'),	(91181,'Maggie'),	(91254,'Maggie'),	(91272,'Josephine'),	(91303,'Josephine'),	(91313,'Josephine'),	(91351,'Josephine'),	(91357,'Josephine'),	(91390,'Josephine'),	(91413,'Maggie'),	(91458,'Maggie'),	(91483,'Josephine'),	(91510,'Josephine'),	(91597,'Josephine'),	(91728,'Maggie'),	(91884,'Josephine'),	(91899,'Josephine'),	(91904,'Maggie'),	(92020,'Maggie'),	(92021,'Maggie'),	(92049,'Maggie'),	(92065,'Maggie'),	(92151,'Josephine'),	(92209,'Josephine'),	(92215,'Maggie'),	(92242,'Maggie'),	(92255,'Maggie'),	(92263,'Maggie'),	(92427,'Maggie'),	(92449,'Maggie'),	(92595,'Maggie'),	(92677,'Maggie'),	(92682,'Maggie'),	(92688,'Maggie'),	(92715,'Maggie'),	(92718,'Maggie'),	(92731,'Maggie'),	(92738,'Maggie'),	(92771,'Maggie'),	(92857,'Maggie'),	(92862,'Maggie'),	(93108,'Josephine'),	(93133,'Maggie'),	(93208,'Maggie'),	(93218,'Maggie'),	(93256,'Josephine'),	(93313,'Josephine'),	(93347,'Maggie'),	(93351,'Josephine'),	(93384,'Maggie'),	(93395,'Josephine'),	(93488,'Maggie'),	(93498,'Maggie'),	(93681,'Josephine'),	(93695,'Josephine'),	(93700,'Maggie'),	(93753,'Maggie'),	(93765,'Maggie'),	(93771,'Maggie'),	(93785,'Josephine'),	(93786,'Maggie'),	(93934,'Josephine'),	(93949,'Maggie'),	(93976,'Maggie'),	(94122,'Maggie'),	(94150,'Maggie'),	(94266,'Maggie'),	(94341,'Maggie'),	(94352,'Maggie'),	(94427,'Maggie'),	(94434,'Maggie'),	(94455,'Maggie'),	(94533,'Maggie'),	(94562,'Maggie'),	(94605,'Maggie'),	(94619,'Maggie'),	(94621,'Maggie'),	(94674,'Maggie'),	(94680,'Maggie'),	(94799,'Maggie'),	(94855,'Maggie'),	(94869,'Maggie'),	(94937,'Maggie'),	(94957,'Maggie'),	(94962,'Maggie'),	(95073,'Maggie'),	(95082,'Maggie'),	(95141,'Maggie'),	(95145,'Maggie'),	(95216,'Maggie'),	(95256,'Maggie'),	(95930,'Maggie'), (78861,'Maggie'), (67139, 'Maggie'), (95168, 'Maggie'), (86781,'Maggie')
    ) AS loans_in_digital (loan_id,officer)
),payments AS (
    SELECT 
        lp.loan_id,to_eat(paid_at) paid_at,amount,principal_amount,interest_amount,fees_amount
    FROM loan_loanpayment lp 
    LEFT JOIN got_to_collections gt ON gt.loan_id = lp.loan_id 
    WHERE trashed_at isnull
        AND paid_at >= entered_collections_at
        AND note NOT like '%penalty waiver%'
),months AS  (
    SELECT 
        generate_series('2020-01-01 00:00:00'::TIMESTAMP WITHOUT time ZONE, eat_day(now()), '1 day'::interval) AS start_of_day,
        ((generate_series('2020-01-01 00:00:00'::TIMESTAMP WITHOUT time ZONE, eat_day(now()), '1 day'::interval) + '1 day'::interval) - '00:00:01'::interval) AS end_of_day 
),month_loans AS  (
  SELECT loan_id,
          months.start_of_day,
          months.end_of_day
   FROM got_to_collections gt, months
   WHERE (gt.entered_collections_at < months.end_of_day)
          AND ((completed_at IS NULL)
               OR (completed_at + '2 day'::interval > months.end_of_day)) --- to capture the transaction when the client pays up in collections
          AND ((written_off_at IS NULL)
               OR (written_off_at > months.end_of_day))
),loans_with_payment_info AS (      
    SELECT distinct on(ml.loan_id, p.paid_at)
                        ml.loan_id,
                        start_of_day,
                        end_of_day,
                        paid_at ,
                        amount AS total_paid,
                        principal_amount  AS principal_paid
    FROM month_loans ml
    LEFT JOIN loan_loan l ON l.id = ml.loan_id 
    LEFT JOIN payments p ON p.loan_id = l.id
    WHERE paid_at <= ml.end_of_day
    ORDER BY ml.loan_id,p.paid_at  desc
),results AS (
    SELECT
        paid_at::date AS day,
        business_id,
        business_name,
        l.loan_id,
        loan_name,
        l.total_paid,
        days_in_arrears,
        written_off_at,
        officer
    FROM loans_with_payment_info l
    LEFT JOIN loans_in_collections on loans_in_collections.loan_id = l.loan_id
    LEFT JOIN got_to_collections gt on gt.loan_id = loans_in_collections.loan_id
),details AS (
SELECT
    day AS payment_date,
    officer,
    business_id,
    business_name,
    loan_id,
    loan_name,
    total_paid
FROM results

),

MTD_Collections AS (select
       officer,
       date_trunc('month',payment_date) mtd_month,
       SUM(total_paid) total_collected
from details where date_trunc('month',payment_date) = date_trunc('month',to_eat(now()))

group by 1,2),

DC_Calls AS (
SELECT c.loan_application_id loan_id
      ,c.attempted_by_id
      ,c.contacted_successfully
      ,(CASE 
          WHEN c.attempted_by_id = 137995 THEN 'Josephine'
          WHEN c.attempted_by_id = 17293 THEN 'Maggie'
       END) officer
      ,date_trunc('day',to_eat(c.attempted_at)) attempted_at
   FROM loan_management_clientcontactattempt c
   WHERE c.attempted_by_id in (137995,17293)
   ),
   
   writeoffs as (
    select 
        loan_loan.id as loan_id,
        to_eat(due_date) due_date,
        to_eat(written_off_at) written_off_at,
        extract(day from (now() - due_date)) days_in_arrears,
        to_eat(completed_at) completed_at,
        _db_total_late_fees,
        business_id,
        payment_status,
        username,
        to_eat(closed_out_at) closed_out_at
    from loan_loan 
    left join auth_user on auth_user.id = field_officer_id
    where written_off_at notnull
)

,dc_payments as (
    select
        loan_id,to_eat(paid_at) paid_at,amount,principal_amount,interest_amount,fees_amount,note,penalties_amount
    from loan_loanpayment 
    where trashed_at isnull    
)
,MTD_FieldInvestigation AS (
    select
        (CASE WHEN username = 'eomwatum' THEN 'Emma'
              WHEN username = 'jwamala' THEN 'Joseph'
              WHEN username = 'pobwana' THEN 'Paul'
         END) officer,
        date_trunc ('month',paid_at) mtd_month, 
        sum(principal_amount) collected_principal,
        sum(interest_amount)  collected_interest,
        sum(penalties_amount) collected_penalties,
        sum(fees_amount)      collected_fees,
        sum(amount)           collected_overall
    from dc_payments
     left join writeoffs on writeoffs.loan_id = dc_payments.loan_id
    where paid_at >= written_off_at
    group by 1,2
),

FI_Calls AS (
SELECT (CASE 
          WHEN c.attempted_by_id = 85898 THEN 'Emma'
          WHEN c.attempted_by_id = 19560 THEN 'Paul'
          WHEN c.attempted_by_id = 106652 THEN 'Joseph'
       END) officer
      ,date_trunc('month',to_eat(c.attempted_at)) attempted_at
      ,count( distinct c.loan_application_id) mtd_total_customers
      ,count(*) mtd_contact_attempts
     , count(*) FILTER (WHERE contacted_successfully IS TRUE) mtd_successful_contacts
     , count(*) FILTER (WHERE date_trunc('day',to_eat(c.attempted_at)) = date_trunc('day',to_eat(CURRENT_DATE))) contact_attempts_today
   FROM loan_management_clientcontactattempt c
   WHERE c.attempted_by_id in (85898,19560,106652) AND  date_trunc('month',to_eat(c.attempted_at)) = date_trunc('month',to_eat(CURRENT_DATE)) 
   group by 1,2
   ),
 
 Combined_Table AS (
Select 'FI' team
     , mtd.officer
     , collected_overall::bigint mtd_collected
     , mtd_total_customers
     , mtd_contact_attempts
     , mtd_successful_contacts
     , contact_attempts_today
     from MTD_FieldInvestigation mtd inner join FI_Calls c on mtd.officer = c.officer 
     AND mtd_month = date_trunc('month',to_eat(c.attempted_at))
     WHERE mtd.officer notnull
union all
Select 'DC' team
     , mtd.officer
     , SUM(DISTINCT mtd.total_collected)::bigint mtd_collected
     , count( distinct c.loan_id) mtd_total_customers
     , count(*) mtd_contact_attempts
     , count(*) FILTER (WHERE c.contacted_successfully IS TRUE) mtd_successful_contacts
     , count(*) FILTER (WHERE date_trunc('day',to_eat(c.attempted_at)) = date_trunc('day',to_eat(CURRENT_DATE))) contact_attempts_today
     from MTD_Collections mtd left join DC_Calls c on mtd.officer = c.officer 
     AND mtd_month = date_trunc('month',to_eat(c.attempted_at))
     
     GROUP BY 1,2
)

Select * from Combined_Table 
union 
Select 'Teams'
, 'Total'
, SUM (mtd_collected)::bigint mtd_collected
, SUM (mtd_total_customers)::bigint mtd_total_customers
, SUM (mtd_contact_attempts)::bigint mtd_contact_attempts
, SUM (mtd_successful_contacts)::bigint mtd_successful_contacts
, SUM (contact_attempts_today)::bigint contact_attempts_today
from Combined_Table
order by 1,2