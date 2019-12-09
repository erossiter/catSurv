#' MTurk Political Knowledge
#'
#' Data from Amazon Mechanical Turkers in 2012 responding to 64 political knowledge questions.  
#' Political knowledge questions had a varying number of response options, noted below.  
#' This dataset codes answers as either correct (a value of 0) or incorrect (a value of 1).
#' 
#' @format A data frame with 810 observations on the following 64 questions.
#' \describe{
#' 
#' \item{\code{Q1}}{How long is one term for the President of the United States?
#' 
#' Eight years; Six years; Four years; Two years}
#' 
#' \item{\code{Q2}}{The FDA is part of the national government primarily responsible for regulating
#'   
#'   Food quality; The national parks; Electricity production and energy; Pollution and the environment}
#' 
#' \item{\code{Q3}}{Who is the Vice President of the United States?
#'   
#'   Leon Panetta; William Daley; Hillary Clinton; Joe Biden}
#' 
#' \item{\code{Q4}}{The federal debt is 
#'   
#'   Much smaller than it was 20 years ago; The difference between imports and exports with foreign countries; 
#'   The annual difference between spending and tax revenues; The accumulated borrowing 
#'   of the federal government that has not been repaid}
#' 
#' \item{\code{Q5}}{How many times can an individual be elected President of the United States under current laws?
#'   
#'   Any number of terms; Three times; Twice; Once}
#' 
#' \item{\code{Q6}}{What do we call the first ten amendments to the Constitution?
#'   
#'   The Articles of Confederation; The inalienable right; The Bill of Rights; The Declaration of Independence}
#' 
#' \item{\code{Q7}}{Is the U.S. federal budget deficit, the amount by which the governments 
#' spending exceeds the amount of money it collects, now bigger, about the same, or smaller 
#' than it was during most of the 1990s?
#'   
#'   Smaller; About the same; Bigger}
#' 
#' \item{\code{Q8}}{Who signs bills to become laws?
#'   
#'   The President; The Vice President; The Chief Justice of the Supreme Court; The Secretary of State}
#' 
#' \item{\code{Q9}}{Which party is generally more supportive of creating a way for immigrants who 
#' are in the U.S. illegally to eventually become citizens?
#'   
#'   The Republican Party; The Democratic Party}
#' 
#' \item{\code{Q10}}{In what month do we vote for the President?
#'   
#'   November; October; February; January}
#' 
#' \item{\code{Q11}}{What are the two parts of the U.S. Congress?
#'   
#'   The Senate and the Supreme Court; The House of Lords and the House of Commons; The House of 
#'   Representative and the Supreme Court; The Senate and House of Representatives}
#' 
#' \item{\code{Q12}}{Which party is generally more supportive of restricting access to abortion?
#'   
#'   The Republican Party; The Democratic Party}
#' 
#' \item{\code{Q13}}{Which of these political parties is considered most conservative?
#'   
#'   Green Party; Republican Party; Democratic Party}
#' 
#' \item{\code{Q14}}{The NRA is an organization that advocates for 
#'   
#'   Clean elections; A cleaner environment; The rights of gun owners; Women's rights}
#'   
#'   \item{\code{Q15}}{Compared to 30 years ago, is the difference in incomes between the top 
#'   20\% of households and the bottom 20\% of households now bigger, smaller, or the same?
#'   
#'   The difference is now the same as 30 years ago; The difference is now smaller than 30 years ago; 
#'   The difference is now bigger than 30 years ago}
#'   
#'   \item{\code{Q16}}{The EPA is part of the national government primarily responsible for regulating
#'   
#'   Food quality; The national parks; Electricity production and energy; Pollution and the environment}
#'   
#'   \item{\code{Q17}}{Which party is generally more supportive of reducing the size of the defense budget?
#'   
#'   The Republican Party; The Democratic Party}
#'   
#'   \item{\code{Q18}}{Which party is generally more supportive of increasing taxes on higher 
#'   income people to reduce the federal budget deficit?
#'   
#'   The Republican Party; The Democratic Party}
#'   
#'   \item{\code{Q19}}{Which party is generally more supportive of allowing drilling for oil in 
#'   the Arctic National Wildlife Refuge?
#'   
#'   The Republican Party; The Democratic Party}
#'   
#'   \item{\code{Q20}}{Who is the Commander in Chief of the military?
#'   
#'   The Attorney General; The President; The Secretary of Defense; The Vice President}
#'   
#'   \item{\code{Q21}}{The First Amendment to the United States Constitution guarantees all of these rights EXCEPT
#'   
#'   Right to remain silent; Right to the free exercise of religion; Right to free speech; Right to peaceably assemble}
#'   
#'   \item{\code{Q22}}{Roe v Wade is a case decided by the Supreme Court that relates to
#'   
#'   Executive power; Campaign finance; Birth control; Abortion}
#'   
#'   \item{\code{Q23}}{Social Security is
#'   
#'   Funded by the personal income tax; Operated by state government; The responsibility of the 
#'   Department of Defense; The benefit program for senior citizens}
#'   
#'   \item{\code{Q24}}{What is Medicare?
#'   
#'   A private, non-profit organization that runs free health clinic; A private health insurance 
#'   plan sold to individuals in all 50 states; A program run by state governments to provide health 
#'   care to poor people; A program run by the U.S. federal government to pay for old people's health care}
#' 
#' \item{\code{Q25}}{How many senators are elected from each state?
#'   
#'   It depends on the population of the state; Four; Two; One}
#' 
#' \item{\code{Q26}}{How many votes are required in Congress to override a presidential veto
#'   
#'   A simple majority of both houses of Congress; A simple majority of one house of Congress; 
#'   A two-thirds majority of both houses of Congress; A two-thirds majority of one house of Congress}
#' 
#' \item{\code{Q27}}{The Secretary of State
#'   
#'   Serves a two-year term; Serves the state governments; Is nominated by the president; Heads 
#'   the armed services}
#' 
#' \item{\code{Q28}}{Near the end of an election campaign, a polls shows that an issue that no 
#' candidate has mentioned is of great concern to voters. What is most likely to happen?
#'   
#'   Some candidates will drop out of the race; Candidates will start talking about the issue; 
#'   Newspapers will not report the results of the poll; The election will be postponed}
#' 
#' \item{\code{Q29}}{Liberals are generally said to
#'   
#'   Support pro-life policies; Oppose all tax increases; Support military spending; Support 
#'   government programs to give government assistance the needy}
#' 
#' \item{\code{Q30}}{Which party is generally more supportive of reducing the size and scope 
#' of the federal government?
#'   
#'   The Republican Party; The Democratic Party}
#' 
#' \item{\code{Q31}}{The ability of a minority of senators to prevent a vote on a bill 
#' is known as Suspension of the rules; Enrollment; A veto; A filibuster}
#' 
#' \item{\code{Q32}}{conservatives are generally said to
#'   
#'   Support pro-choice policies; Support tax cuts; Oppose military spending; 
#'   Support government programs to give government assistance to the needy}
#' 
#' \item{\code{Q33}}{Which of these regions of the country is generally 
#' considered to be most supportive of Republican candidates 
#'   
#'   Midwest; South; West Coast; New England}
#' 
#' \item{\code{Q34}}{The presiding officer in the House of Representatives is 
#'   
#'   The Majority Leader; The Sergeant at Arms; The Vice President of the 
#'   United States; The Speaker}
#' 
#' \item{\code{Q35}}{Which of these countries is NOT a permanent member of 
#' the U.N. Security Council
#'   
#'   United Kingdom; France; India; China}
#' 
#' \item{\code{Q36}}{Which part has a majority of seats in the U.S. House 
#' of Representatives?
#'   
#'   Neither; Democrats; Republicans}
#' 
#' \item{\code{Q37}}{What state holds the first primary election in 
#' Presidential primaries?
#'   
#'   Florida; Nevada; South Carolina; New Hampshire}
#' 
#' \item{\code{Q38}}{Who is the Speaker of the House of Representatives?
#'   
#'   Mitt Romney; Eric Holder; Harry Reid; John Boehner}
#' 
#' \item{\code{Q39}}{Most cases are considered by the Supreme Court
#'   
#'   In even-numbered years; At the request of the Congress; Upon order 
#'   of the president; With the approval of at least four justices}
#' 
#' \item{\code{Q40}}{How many Justices typically serve on the U.S. Supreme Court
#'   
#'   Eleven; Nine; Eight; Seven}
#' 
#' \item{\code{Q41}}{What job or political office is no held by Ben Bernanke?
#'   
#'   None of these; Minority Whip of the U.S. House; Chief Justice of 
#'   the United States Supreme Court; Majority leader of the U.S. Senate, Chairman of the Federal Reserve}
#' 
#' \item{\code{Q42}}{Whose responsibility is it to nominate judges to the Federal Courts
#'   
#'   The state governors; The Supreme Court; Congress; President}
#' 
#' \item{\code{Q43}}{Who is the Chief Justice of the U.S. Supreme Court?
#'   
#'   Larry Thompson; Anthony Kennedy; David Cole; John Roberts}
#' 
#' \item{\code{Q44}}{The U.S. Senate 
#'   
#'   Votes to confirm nominees to the U.S. Supreme court chosen by the 
#'   House of Representatives; Plays no role in choosing the members 
#'   of the U.S. Supreme Court; Chooses members of the U.S. Supreme 
#'   Court; Votes to confirm nominees to the U.S. Supreme Court chosen by the President}
#' 
#' \item{\code{Q45}}{Which party has a majority of seats in the U.S. Senate
#'   
#'   Neither; Democrats; Republicans}
#' 
#' \item{\code{Q46}}{Which of the states listed below has the 
#' greatest number of electoral college votes in the U.S. Presidential Elections?
#'   
#'   Puerto Rico; Nevada; North Dakota; Washington, D.C.}
#' 
#' \item{\code{Q47}}{Citizens United v the FEC is a case decided 
#' by the Supreme Court that relates to
#'   
#'   Executive power; Campaign finance; Birth control; Abortion}
#' 
#' \item{\code{Q48}}{For how many years is a United States Senator 
#' elected that is, how many years are there in one full term of office for a U.S. Senator?
#'   
#'   None of these; Eight years; Six years; Four years; Two years}
#' 
#' \item{\code{Q49}}{Who is the Prime Minister of the United Kingdom?
#'   
#'   Richard Branson; Tony Hayward; Nick Clegg; David Cameron}
#' 
#' \item{\code{Q50}}{The president of Afghanistan is named
#'   
#'   Bashar al-Assad; Hosni Mubarak; Hamid Karzai; Nouri al-Maliki}
#' 
#' \item{\code{Q51}}{The House of Representatives has how many voting members?
#'   
#'   Four hundred and forty-one; Four hundred and thirty-five; 
#'   Two hundred; One hundred}
#' 
#' \item{\code{Q52}}{The President of the Senate is
#'   
#'   The Majority Leader; The Sergeant at Arms; The Vice President 
#'   of the United States; The senior senator of the majority party}
#' 
#' \item{\code{Q53}}{On which of the following federal programs is 
#' the most money spent each year?
#'   
#'   Medicare; Education; Subsidies to farmers; Aid to foreign countries}
#' 
#' \item{\code{Q54}}{What do all constitutional governments have?
#'   
#'   Separation of church and state; A bill of rights; A President 
#'   as the head of government; Limits on political power}
#' 
#' \item{\code{Q55}}{One which of the following does the U.S. 
#' federal government spend the least money?
#'   
#'   Social Security; National defense; Medicare; Foreign aid}
#' 
#' \item{\code{Q56}}{The head of the Department of Justice is
#'   
#'   Kathleen Sebelius; Eric Holder; Timothy Geithner; Hillary Clinton}
#' 
#' \item{\code{Q57}}{The president may NOT
#'   
#'   Declare war; Pardon criminals without justification; Appoint 
#'   federal officials when Congress is in recess; Refuse to sign 
#'   legislation passed by Congress}
#' 
#' \item{\code{Q58}}{Which of these is NOT primarily the 
#' responsibility of the Federal government in Washington?
#'   
#'   Interstate commerce; Negotiating treaties with foreign countries; 
#'   Education; National defense}
#' 
#' \item{\code{Q59}}{Who is the current president of Mexico?
#'   
#'   Vincente Fox; Hugo Chavez; Dilma Rousseff; Felipe Calderon}
#' 
#' \item{\code{Q60}}{Which of the following actions does the United 
#' States federal government commonly take to finance a budget deficit?
#'   
#'   Expanding public-works projects; Borrowing from the public; 
#'   Imposing import quotas; Printing more money}
#' 
#' \item{\code{Q61}}{Common Cause is an organization that advocates for
#'   
#'   Women's rights; Clean elections; A cleaner environment; The right of gun owners}
#'   
#'   \item{\code{Q62}}{The Byrd Rule is relevant
#'   
#'   During the confirmation of cabinet members; For national party conventions; 
#'   During Congressional debates over non-budgetary policies; For the 
#'   Reconciliation process}
#'   
#'   \item{\code{Q63}}{The Majority Leader of the House of Representative is
#'   
#'   Nancy Pelosi; Kevin McCarthy; Eric Cantor; John Boehner}
#'   
#'   \item{\code{Q64}}{On which of the following does the U.S. federal 
#'   government spend the most money each year?
#'   
#'   Education; Medicare; Interest on the national debt; National defense}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name polknowMT
#' @usage data(polknowMT)
#'
#' @references Jacob M. Montgomery and Joshua Cutler. 2013. 
#' "Computerized Adaptive Testing for Public Opinion Surveys." 
#' Political Analysis (Spring 2013) 21 (2): 172-192.
#' 
#' @source \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/19381}
"polknowMT"





#' TAPS Political Knowledge
#'
#' Data of responses to a political knowledge battery from the May 2013 wave 
#' of The American Panel Survey (TAPS) out of Washington University in St. Louis.  
#' 
#' TAPS is a monthly online panel survey  of about 2,000 adults in the United 
#' States. The panel was recruited in the fall of 2011 using an address-based 
#' sampling frame.  TAPS surveys are administered online.  Selected panelists 
#' who do not have a computer or online service are provided a computer and 
#' internet access by TAPS. 
#' 
#' @format A data frame with 1496 observations on the following 10 questions.
#' \describe{
#' 
#' \item{\code{Q1}}{Members of the U.S. Supreme Court serve 
#' 
#' 1 = two-year terms. 2 = ten-year terms. 3 = life terms. 4 = terms determined by the president. 5 = Don't know.
#' }
#' 
#' \item{\code{Q2}}{Who is the Chief Justice of the U.S. Supreme Court?
#' 
#' 1 = John Roberts. 2 = Antonin Scalia. 3 = Mitt Romney. 4 = Hillary Clinton. 5 = Don't know.
#' }
#' 
#' \item{\code{Q3}}{Social Security is
#' 
#' 1 = the benefit program for senior citizens. 2 = the responsibility of the 
#' Department of Defense. 3 = operated by state governments. 4 = funded by the 
#' personal income tax. 5 = Don't know.
#' }
#' 
#' \item{\code{Q4}}{On which of the following programs is the most money spent each year?
#' 
#' 1 = aid to foreign countries. 2 = Medicare. 3 = subsidies to farmers. 4 = education. 5 = Don't know.
#' }
#' 
#' \item{\code{Q5}}{Which party holds a majority of seats in the U.S. House of Representatives in Washington?
#' 
#' 1 = Democrats. 2 = Republicans. 3 = Independents. 4 = Don't know.
#' }
#' 
#' \item{\code{Q6}}{How many votes are required in Congress to override a presidential veto?
#' 
#' 1 = a simple majority of one house of Congress. 2 = a simple majority of both 
#' houses of Congress. 3 = a two-thirds majority of one house of Congress. 4 = a 
#' two-thirds majority of both houses of Congress. 5 = Don't know.
#' }
#' 
#' \item{\code{Q7}}{How long is one term for a member of the U.S. Senate?
#' 
#' 1 = two years. 2 = four years. 3 = six years. 4 = eight years. 5 = Don't know.
#' }
#' 
#' \item{\code{Q8}}{The ability of a minority of senators to prevent a vote on a bill is known as 
#' 
#' 1 = a veto. 2 = a filibuster. 3 = enrollment. 4 = suspension of the rules. 5 = Don't know.
#' }
#' 
#' \item{\code{Q9}}{Who is the Vice President of the United States?
#' 
#' 1 = Nancy Pelosi. 2 = John Boehner. 3 = Joseph Biden. 4 = Harry Reid. 5 = Don't know.
#' }
#' 
#' \item{\code{Q10}}{A president may serve
#' 
#' 1 = one term. 2 = two terms. 3 = three terms. 4 = any number of terms. 5 = Don't know.
#' }
#' }
#'
#' @docType data
#' @keywords datasets
#' @name polknowTAPS
#' @usage data(polknowTAPS)
#'
#' 
#' @source \url{https://wc.wustl.edu/}
"polknowTAPS"




#' TAPS Political Knowledge (Ordered Response Options)
#'
#' Data of responses to a political knowledge battery from the May 2013 wave of 
#' The American Panel Survey (TAPS) out of Washington University in St. Louis.  
#' 
#' TAPS is a monthly online panel survey  of about 2,000 adults in the United 
#' States. The panel was recruited in the fall of 2011 using an address-based 
#' sampling frame.  TAPS surveys are administered online.  Selected panelists 
#' who do not have a computer or online service are provided a computer and 
#' internet access by TAPS. 
#' 
#' @format A data frame with 1340 observations (only complete cases) of the following 10 questions.
#' \describe{
#' 
#' \item{\code{Q1}}{Members of the U.S. Supreme Court serve 
#' 
#' 3 = two-year terms. 4 = ten-year terms. 5 = life terms. 2 = terms determined by the president. 1 = Don't know.
#' }
#' 
#' \item{\code{Q2}}{Who is the Chief Justice of the U.S. Supreme Court?
#' 
#' 5 = John Roberts. 4 = Antonin Scalia. 3 = Mitt Romney. 2 = Hillary Clinton. 1 = Don't know.
#' }
#' 
#' \item{\code{Q3}}{Social Security is
#' 
#' 5 = the benefit program for senior citizens. 2 = the responsibility of the 
#' Department of Defense. 3 = operated by state governments. 4 = funded by the personal income tax. 1 = Don't know.
#' }
#' 
#' \item{\code{Q4}}{On which of the following programs is the most money spent each year?
#' 
#' 4 = aid to foreign countries. 5 = Medicare. 2 = subsidies to farmers. 3 = education. 1 = Don't know.
#' }
#' 
#' \item{\code{Q5}}{Which party holds a majority of seats in the U.S. House of Representatives in Washington?
#' 
#' 4 = Democrats. 3 = Republicans. 2 = Independents. 1 = Don't know.
#' }
#' 
#' \item{\code{Q6}}{How many votes are required in Congress to override a presidential veto?
#' 
#' 2 = a simple majority of one house of Congress. 3 = a simple majority of both 
#' houses of Congress. 4 = a two-thirds majority of one house of Congress. 5 = a 
#' two-thirds majority of both houses of Congress. 1 = Don't know.
#' }
#' 
#' \item{\code{Q7}}{How long is one term for a member of the U.S. Senate?
#' 
#' 3 = two years. 4 = four years. 5 = six years. 2 = eight years. 1 = Don't know.
#' }
#' 
#' \item{\code{Q8}}{The ability of a minority of senators to prevent a vote on a bill is known as 
#' 
#' 4 = a veto. 5 = a filibuster. 3 = enrollment. 2 = suspension of the rules. 1 = Don't know.
#' }
#' 
#' \item{\code{Q9}}{Who is the Vice President of the United States?
#' 
#' 4 = Nancy Pelosi. 3 = John Boehner. 5 = Joseph Biden. 2 = Harry Reid. 1 = Don't know.
#' }
#' 
#' \item{\code{Q10}}{A president may serve
#' 
#' 3 = one term. 5 = two terms. 4 = three terms. 2 = any number of terms. 1 = Don't know.
#' }
#' }
#'
#' @docType data
#' @keywords datasets
#' @name polknowOrdered
#' @usage data(polknowOrdered)
#'
#' 
#' @source \url{https://wc.wustl.edu/}
"polknowOrdered"






#' Narcissistic Personality Inventory
#'
#' Data from the 40 item Narcissistic Personality Inventory (Raskin and Terry 1988) collected by
#' by the Open Source Psychometrics Project in 2012.
#' 
#' Item responses are paired. Respondents had to choose the one that fit them the best.
#' 
#' @format A data frame with 11,243 observations on the following 40 variables.
#' \describe{
#' \item{\code{Q1}}{0 = I have a natural talent for influencing people. 1 = I am not good at influencing people.}
#' \item{\code{Q2}}{0 = Modesty doesn't become me. 1 = I am essentially a modest person.}
#' \item{\code{Q3}}{0 = I would do almost anything on a dare. 1 = I tend to be a fairly cautious person.}
#' \item{\code{Q4}}{0 = When people compliment me I sometimes get embarrassed. 1 = I know that I am good because everybody keeps telling me so.}
#' \item{\code{Q5}}{0 = The thought of ruling the world frightens the hell out of me. 1 = If I ruled the world it would be a better place.}
#' \item{\code{Q6}}{0 = I can usually talk my way out of anything. 1 = I try to accept the consequences of my behavior.}
#' \item{\code{Q7}}{0 = I prefer to blend in with the crowd. 1 = I like to be the center of attention.}
#' \item{\code{Q8}}{0 = I will be a success. 1 = I am not too concerned about success.}
#' \item{\code{Q9}}{0 = I am no better or worse than most people. 1 = I think I am a special person.}
#' \item{\code{Q10}}{0 = I am not sure if I would make a good leader. 1 = I see myself as a good leader.}
#' \item{\code{Q11}}{0 = I am assertive. 1 = I wish I were more assertive.}
#' \item{\code{Q12}}{0 = I like to have authority over other people. 1 = I don't mind following orders.}
#' \item{\code{Q13}}{0 = I find it easy to manipulate people. 1 = I don't like it when I find myself manipulating people.}
#' \item{\code{Q14}}{0 = I insist upon getting the respect that is due me. 1 = I usually get the respect that I deserve.}
#' \item{\code{Q15}}{0 = I don't particularly like to show off my body. 1 = I like to show off my body.}
#' \item{\code{Q16}}{0 = I can read people like a book. 1 = People are sometimes hard to understand.}
#' \item{\code{Q17}}{0 = If I feel competent I am willing to take responsibility for making decisions. 1 = I like to take responsibility for making decisions.}
#' \item{\code{Q18}}{0 = I just want to be reasonably happy. 1 = I want to amount to something in the eyes of the world.}
#' \item{\code{Q19}}{0 = My body is nothing special. 1 = I like to look at my body.}
#' \item{\code{Q20}}{0 = I try not to be a show off. 1 = I will usually show off if I get the chance.}
#' \item{\code{Q21}}{0 = I always know what I am doing. 1 = Sometimes I am not sure of what I am doing.}
#' \item{\code{Q22}}{0 = I sometimes depend on people to get things done. 1 = I rarely depend on anyone else to get things done.}
#' \item{\code{Q23}}{0 = Sometimes I tell good stories. 1 = Everybody likes to hear my stories.}
#' \item{\code{Q24}}{0 = I expect a great deal from other people. 1 = I like to do things for other people.}
#' \item{\code{Q25}}{0 = I will never be satisfied until I get all that I deserve. 1 = I take my satisfactions as they come.}
#' \item{\code{Q26}}{0 = Compliments embarrass me. 1 = I like to be complimented.}
#' \item{\code{Q27}}{0 = I have a strong will to power. 1 = Power for its own sake doesn't interest me.}
#' \item{\code{Q28}}{0 = I don't care about new fads and fashions. 1 = I like to start new fads and fashions.}
#' \item{\code{Q29}}{0 = I like to look at myself in the mirror. 1 = I am not particularly interested in looking at myself in the mirror.}
#' \item{\code{Q30}}{0 = I really like to be the center of attention. 1 = It makes me uncomfortable to be the center of attention.}
#' \item{\code{Q31}}{0 = I can live my life in any way I want to. 1 = People can't always live their lives in terms of what they want.}
#' \item{\code{Q32}}{0 = Being an authority doesn't mean that much to me. 1 = People always seem to recognize my authority.}
#' \item{\code{Q33}}{0 = I would prefer to be a leader. 1 = It makes little difference to me whether I am a leader or not.}
#' \item{\code{Q34}}{0 = I am going to be a great person. 1 = I hope I am going to be successful.}
#' \item{\code{Q35}}{0 = People sometimes believe what I tell them. 1 = I can make anybody believe anything I want them to.}
#' \item{\code{Q36}}{0 = I am a born leader. 1 = Leadership is a quality that takes a long time to develop.}
#' \item{\code{Q37}}{0 = I wish somebody would someday write my biography. 1 = I don't like people to pry into my life for any reason.}
#' \item{\code{Q38}}{0 = I get upset when people don't notice how I look when I go out in public. 1 = I don't mind blending into the crowd when I go out in public.}
#' \item{\code{Q39}}{0 = I am more capable than other people. 1 = There is a lot that I can learn from other people.}
#' \item{\code{Q40}}{0 = I am much like everybody else. 1 = I am an extraordinary person.}
#' }
#' 
#' @references 
#' 
#' Raskin, Robert and Howard Terry. 1988. "A Principal-Components Analysis of the Narcissistic Personality Inventory and Further Evidence of Its Construct Validity." Journal of Personality and Social Psychology 54(5):890-902.
#'
#' Open Source Psychometrics Project. \url{https://openpsychometrics.org/_rawdata/}
#'
#' @docType data
#' @keywords datasets
#' @name npi
#' @usage data(npi)
"npi"




#' Need For Cognition
#'
#' Data of survey respondents' responses to 18 NFC questions, which is a reduced version of NFC (Cacioppo and Petty 1984). For each question, respondents could choose one of five response options: 1 = agree strongly, 2 = agree somewhat, 3 = neither agree nor disagree, 4 = disagree somewhat, 5 = disagree strongly. Missingness is optional.
#' Observations come from one of three administrations of the survey: The American Panel Survey, Amazon's Mechanical Turk  in fall of 2014, or Amazon's Mechanical Turk in spring of 2015.
#' 
#' 
#' @format A data frame with 4043 observations on the following 18 variables.
#' \describe{
#'     \item{\code{NFC1}}{I really enjoy a task that involves coming up with new solutions to problems}
#'     \item{\code{NFC4}}{I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought}
#'     \item{\code{NFC10}}{Learning new ways to think doesn't excite me very much}
#'     \item{\code{NFC12}}{I usually end up deliberating about issues even when they do not affect me personally}
#'     \item{\code{NFC15}}{The idea of relying on thought to make my way to the top appeals to me}
#'     \item{\code{NFC16}}{The notion of thinking abstractly is appealing to me}
#'     \item{\code{NFC19}}{I only think as hard as I have to}
#'     \item{\code{NFC21}}{I think tasks that require little thought once I've learned them}
#'     \item{\code{NFC22}}{I prefer to think about small, daily projects to long-term ones}
#'     \item{\code{NFC23}}{I would rather do something that requires little thought than something that is sure to challenge my thinking abilities}
#'     \item{\code{NFC24}}{I find satisfaction in deliberating hard and for long hours}
#'     \item{\code{NFC29}}{I like to have the responsibility of handing a situation that requires a lot of thinkings}
#'     \item{\code{NFC31}}{I feel relief rather than satisfaction after completing a task that required a lot of mental effort}
#'     \item{\code{NFC32}}{Thinking is not my idea of fun}
#'     \item{\code{NFC33}}{I try to anticipate and avoid situations where there is likely a chance I will have to think in depth about something}
#'     \item{\code{NFC39}}{I prefer my life to be filed with puzzles that I must solve}
#'     \item{\code{NFC40}}{I would prefer complex to simple problems}
#'     \item{\code{NFC43}}{Its enough for me that something gets the job done; I don't care how or why it works}
#' }
#' 
#' @references 
#' 
#' Cacioppo, John T. and Richard E. Petty. 1984. "The Efficient Assessment of Need for Cognition." Journal of Personality Assessment 48(3):306-307.
#' 
#' 
#' @docType data
#' @keywords datasets
#' @name nfc
#' @usage data(nfc)
"nfc"




#' Need to Evaluate
#'
#' 4005 full response profiles to 16 Need to Evaluate inventory.
#' Observations come from one of three administrations of the survey: The American Panel Survey, 
#' Amazon's Mechanical Turk  in fall of 2014, or Amazon's Mechanical Turk in spring of 2015.
#' 
#' See \code{\link{nte_cat}} for detailed information on question wordings and response options.
#' 
#' 
#' @seealso \code{\link{nte_cat}}
#'
#' @references 
#' 
#' Jarvis, W. Blair G., and Richard E. Petty.
#' "The need to evaluate." Journal of personality and social psychology 70.1 (1996): 172.
#' 
#' 
#' @docType data
#' @keywords datasets
#' @name nte
#' @usage data(nte)
"nte"






#' ltm Cat Object
#' 
#' An object of class \code{Cat} created using the \code{ltmCat} function with the \code{npi} dataset.
#' 
#' @examples 
#' \dontrun{
#' ## How this Cat object was created
#' data(npi)
#' ltm_cat <- ltmCat(npi, quadraturePoints = 100)
#' }
#' 
#' ## How to load this Cat object for usage
#' data(ltm_cat)
#' 
#' @format An object of class \code{Cat}.  See \code{\link{Cat-class}} for more details.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{ltmCat}}, \code{\link{npi}}  
#'
#' @docType data
#' @name ltm_cat
#' @usage data(ltm_cat)
"ltm_cat"


#' grm Cat Object
#' 
#' An object of class \code{Cat} created using the \code{grmCat} function with the \code{nfc} dataset.
#' 
#' @examples 
#' \dontrun{
#' ## How this Cat object was created
#' data(nfc)
#' grm_cat <- grmCat(nfc, quadraturePoints = 100)
#' }
#' 
#' ## How to load this Cat object for usage
#' data(grm_cat)
#' 
#' @format An object of class \code{Cat}.  See \code{\link{Cat-class}} for more details.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}}, \code{\link{nfc}}  
#'
#' @docType data
#' @name grm_cat
#' @usage data(grm_cat)
"grm_cat"


#' tpm Cat Object
#' 
#' An object of class \code{Cat} created using the \code{tpmCat} function with the first twenty questions of the \code{polknowMT} dataset.
#' 
#' @examples 
#' \dontrun{
#' ## How this Cat object was created
#' data(polknowMT)
#' tpm_cat <- tpmCat(polknowMT[,1:20], quadraturePoints = 100, start.val = "random")
#' }
#' 
#' ## How to load this Cat object for usage
#' data(tpm_cat)
#' 
#' @format An object of class \code{Cat}.  See \code{\link{Cat-class}} for more details.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{tpmCat}}, \code{\link{polknowMT}}  
#'
#' @docType data
#' @name tpm_cat
#' @usage data(tpm_cat)
"tpm_cat"


#' gpcm Cat Object
#' 
#' An object of class \code{Cat} created using the \code{gpcmCat} function with the \code{polknowTAPS} dataset.
#' To have a better fitting model, we first fit an object of class \code{gpcm} from the \code{ltm} package which
#' provides for additional control values to be used in fitting.  See \code{gpcmCat}.
#' 
#' @examples 
#' \dontrun{
#' ## How this Cat object was created
#' data(polknowTAPS)
#' gpcm_fit <- gpcm(polknowTAPS, constraint = "gpcm", control = list(iter.qN = 200, GHk = 100))
#' gpcm_cat <- gpcmCat(gpcm_fit)
#' }
#' 
#' ## How to load this Cat object for usage
#' data(gpcm_cat)
#' 
#' @format An object of class \code{Cat}.  See \code{\link{Cat-class}} for more details.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{gpcmCat}}, \code{\link{polknowTAPS}}  
#'
#' @docType data
#' @name gpcm_cat
#' @usage data(gpcm_cat)
"gpcm_cat"





#' Narcissistic personality inventory question items
#' 
#' A list where each element is a length three vector containin the text of the the question item and the two response options.
#' 
#' @seealso \code{\link{npi_cat}}
#' 
#' @references 
#' Raskin, R., and H. Terry (1988), "A Principal-Components Analysis of the Narcissistic Personality
#' Inventory and Further Evidence of Its Construct Validity," Journal of Personality and Social
#' Psychology, 54, 890-902.
#'
#' 
#' @examples 
#' 
#' data(npi_battery)
#' 
#' # Item 1
#' npi_battery$Q1[1]
#' 
#' # Response options for item 1
#' npi_battery$Q1[2:3]
#' 
#'
#' @docType data
#' @name npi_battery
#' @usage data(npi_battery)
"npi_battery"



#' Agreeableness Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20-item Agreeableness dimension of the 
#' 100-item IPIP representation of Costa and McCrae's Five Factor Model
#' 
#' @details
#' 
#' Cat object containing item parameters for graded response model fit with 774,410 response profiles by myPersonality Poject and 1500 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model. 
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.113) and a standard deviation (1.5)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Very inaccurate; 2=Moderately inaccurate; 3=Neither inaccurate nor accurate;
#' 4=Moderately accurate; 5=Very accurate.
#' 
#' The wording of the question items:
#' 
#' I...
#' \describe{
#' \item{\code{q86}}{Have a good word for everyone.}
#' \item{\code{q6}}{Believe that others have good intentions.}
#' \item{\code{q66}}{Respect others.}
#' \item{\code{q46}}{Accept people as they are.}
#' \item{\code{q36}}{Make people feel at ease.}
#' \item{\code{q26}}{Am concerned about others.}
#' \item{\code{q56}}{Trust what people say.}
#' \item{\code{q76}}{Sympathize with others' feelings.}
#' \item{\code{q13}}{Am easy to satisfy.}
#' \item{\code{q96}}{Treat all people equally.}
#' \item{\code{q82}}{Have a sharp tongue.}
#' \item{\code{q9}}{Cut others to pieces.}
#' \item{\code{q22}}{Suspect hidden motives in others.}
#' \item{\code{q32}}{Get back at others.}
#' \item{\code{q92}}{Insult people.}
#' \item{\code{q42}}{Believe that I am better than others.}
#' \item{\code{q52}}{Contradict others.}
#' \item{\code{q62}}{Make demands on others.}
#' \item{\code{q2}}{Hold a grudge.}
#' \item{\code{q72}}{Am out for my own personal gain.}
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}}
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' @references 
#' Costa, P. T., Jr., & McCrae, R. R. (1992). Revised NEO Personality Inventory (NEO PI-R)
#' and NEO Five-Factor Inventory (NEO-FFI): Professional manual. Odessa, FL: Psychological Assessment Resources.
#'
#'
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#'
#' @docType data
#' @name agree_cat
#' @usage data(agree_cat)
"agree_cat"








#' Neuroticism Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20-item Neuroticism dimension of the 
#' 100-item IPIP representation of Costa and McCrae's Five Factor Model
#' 
#' @details
#' 
#' Cat object containing item parameters for graded response model fit with
#'  774,410 response profiles by myPersonality Project and 1500 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-.025) and a standard deviation (1.2)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Very inaccurate; 2=Moderately inaccurate; 3=Neither inaccurate nor accurate;
#' 4=Moderately accurate; 5=Very accurate.
#' 
#' The wording of the question items:
#' 
#' I...
#' \describe{
#' \item{\code{q12}}{Often feel blue.}
#' \item{\code{q30}}{Dislike myself.}
#' \item{\code{q80}}{Am often down in the dumps.}
#' \item{\code{q70}}{Have frequent mood swings.}
#' \item{\code{q50}}{Panic easily.}
#' \item{\code{q90}}{Am filled with doubts about things.}
#' \item{\code{q100}}{Feel threatened easily.}
#' \item{\code{q17}}{Get stressed out easily.}
#' \item{\code{q40}}{Fear for the worst.}
#' \item{\code{q60}}{Worry about things.}
#' \item{\code{q37}}{Seldom feel blue.}
#' \item{\code{q11}}{Feel comfortable with myself.}
#' \item{\code{q67}}{Rarely get irritated.}
#' \item{\code{q19}}{Am not easily bothered by things.}
#' \item{\code{q97}}{Am very pleased with myself.}
#' \item{\code{q27}}{Am relaxed most of the time.}
#' \item{\code{q77}}{Seldom get mad.}
#' \item{\code{q47}}{Am not easily frustrated.}
#' \item{\code{q87}}{Remain calm under pressure.}
#' \item{\code{q57}}{Rarely lose my composure.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' @references 
#' Costa, P. T., Jr., & McCrae, R. R. (1992). Revised NEO Personality Inventory (NEO PI-R)
#' and NEO Five-Factor Inventory (NEO-FFI): Professional manual. Odessa, FL: Psychological Assessment Resources.
#'
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#'
#' @docType data
#' @name neuro_cat
#' @usage data(neuro_cat)
"neuro_cat"


#' Extraversion Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20-item Extraversion dimension of the 
#' 100-item IPIP representation of Costa and McCrae's Five Factor Model
#' 
#' @details
#' 
#'  Cat object containing item parameters for graded response model fit with
#'   774,410 response profiles by myPersonality Project and 1500 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-.367) and a standard deviation (1.3)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Very inaccurate; 2=Moderately inaccurate; 3=Neither inaccurate nor accurate;
#' 4=Moderately accurate; 5=Very accurate.
#' 
#' The wording of the question items:
#' 
#' I...
#' \describe{
#' \item{\code{q83}}{Feel comfortable around people.}
#' \item{\code{q10}}{Make friends easily.}
#' \item{\code{q73}}{Am skilled in handling social situations.}
#' \item{\code{q63}}{Am the life of the party.}
#' \item{\code{q53}}{Know how to captivate people.}
#' \item{\code{q93}}{Start conversations.}
#' \item{\code{q33}}{Warm up quickly to others.}
#' \item{\code{q43}}{Talk to a lot of different people at parties.}
#' \item{\code{q3}}{Don't mind being the center of attention.}
#' \item{\code{q23}}{Cheer people up.}
#' \item{\code{q39}}{Have little to say.}
#' \item{\code{q14}}{Keep in the background.}
#' \item{\code{q89}}{Would describe my experiences as somewhat dull.}
#' \item{\code{q59}}{Don't like to draw attention to myself.}
#' \item{\code{q29}}{Don't talk a lot.}
#' \item{\code{q18}}{Avoid contacts with others.}
#' \item{\code{q99}}{Am hard to get to know.}
#' \item{\code{q79}}{Retreat from others.}
#' \item{\code{q69}}{Find it difficult to approach others.}
#' \item{\code{q49}}{Keep others at a distance.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#'
#' @references 
#' Costa, P. T., Jr., & McCrae, R. R. (1992). Revised NEO Personality Inventory (NEO PI-R)
#' and NEO Five-Factor Inventory (NEO-FFI): Professional manual. Odessa, FL: Psychological Assessment Resources.
#'
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#'
#'
#' @docType data
#' @name extra_cat
#' @usage data(extra_cat)
"extra_cat"




#' Openness to Experience Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20-item Openness dimension of the 
#' 100-item IPIP representation of Costa and McCrae's Five Factor Model
#' 
#' @details
#' 
#' Cat object containing item parameters for graded response model fit with 774,410 response profiles by myPersonality Project and 1500 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-.674) and a standard deviation (1.2)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Very inaccurate; 2=Moderately inaccurate; 3=Neither inaccurate nor accurate;
#' 4=Moderately accurate; 5=Very accurate.
#' 
#' The wording of the question items:
#' 
#' I...
#' \describe{
#' \item{\code{q91}}{Believe in the importance of art.}
#' \item{\code{q1}}{Have a vivid imagination.}
#' \item{\code{q51}}{Tend to vote for liberal political candidates.}
#' \item{\code{q41}}{Carry the conversation to a higher level.}
#' \item{\code{q61}}{Enjoy hearing new ideas.}
#' \item{\code{q31}}{Enjoy thinking about things.}
#' \item{\code{q21}}{Can say things beautifully.}
#' \item{\code{q16}}{Enjoy wild flights of fantasy.}
#' \item{\code{q81}}{Get excited by new ideas.}
#' \item{\code{q71}}{Have a rich vocabulary.}
#' \item{\code{q24}}{Am not interested in abstract ideas.}
#' \item{\code{q74}}{Do not like art.}
#' \item{\code{q7}}{Avoid philosophical discussions.}
#' \item{\code{q34}}{Do not enjoy going to art museums.}
#' \item{\code{q44}}{Tend to vote for conservative political candidates.}
#' \item{\code{q4}}{Do not like poetry.}
#' \item{\code{q94}}{Rarely look for a deeper meaning in things.}
#' \item{\code{q84}}{Believe that too much tax money goes to support artists.}
#' \item{\code{q54}}{Am not interested in theoretical discussions.}
#' \item{\code{q64}}{Have difficulty understanding abstract ideas.}
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @references 
#' Costa, P. T., Jr., & McCrae, R. R. (1992). Revised NEO Personality Inventory (NEO PI-R)
#' and NEO Five-Factor Inventory (NEO-FFI): Professional manual. Odessa, FL: Psychological Assessment Resources.
#' 
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#' 
#' @docType data
#' @name open_cat
#' @usage data(open_cat)
"open_cat"



#' Conscientiousness Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20-item Conscientiousness dimension of the 
#' 100-item IPIP representation of Costa and McCrae's Five Factor Model
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  774,410 response profiles by myPersonality Project and 1500 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.455) and a standard deviation (1.5)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Very inaccurate; 2=Moderately inaccurate; 3=Neither inaccurate nor accurate;
#' 4=Moderately accurate; 5=Very accurate.
#' 
#' The wording of the question items:
#' 
#' I...
#' \describe{
#' \item{\code{q15}}{Am always prepared.}
#' \item{\code{q85}}{Pay attention to details.}
#' \item{\code{q75}}{Get chores done right away.}
#' \item{\code{q95}}{Carry out my plans.}
#' \item{\code{q55}}{Make plans and stick to them.}
#' \item{\code{q5}}{Complete tasks successfully.}
#' \item{\code{q25}}{Do things according to a plan.}
#' \item{\code{q45}}{Am exacting in my work.}
#' \item{\code{q65}}{Finish what I start.}
#' \item{\code{q35}}{Follow through with my plans.}
#' \item{\code{q28}}{Waste my time.}
#' \item{\code{q88}}{Find it difficult to get down to work.}
#' \item{\code{q98}}{Do just enough work to get by.}
#' \item{\code{q78}}{Don't see things through.}
#' \item{\code{q20}}{Shirk my duties.}
#' \item{\code{q38}}{Mess things up.}
#' \item{\code{q58}}{Leave things unfinished.}
#' \item{\code{q48}}{Don't put my mind on the task at hand.}
#' \item{\code{q68}}{Make a mess of things.}
#' \item{\code{q8}}{Need a push to get started.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @references 
#' Costa, P. T., Jr., & McCrae, R. R. (1992). Revised NEO Personality Inventory (NEO PI-R)
#' and NEO Five-Factor Inventory (NEO-FFI): Professional manual. Odessa, FL: Psychological Assessment Resources.
#'
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#'
#' @docType data
#' @name consc_cat
#' @usage data(consc_cat)
"consc_cat"




#' Empathizing Quotient Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 40-item Empathy Quotient personality inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  13976 response profiles collected by myPersonality Project, 3050 response profiles
#' collected by Qualtrics in June 2018, and 1500 response profiles collected by YouGov in June 2018. 
#' The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-0.363) and a standard deviation (1.5)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Strongly disagree; 2=Slightly disagree; 3=Slightly agree; 4=Strongly agree
#' 
#' The wording of the question items:
#' 
#' \describe{
#' \item{\code{q1}}{ I can easily tell if someone else wants to enter a conversation.}	 							
#' \item{\code{q4}}{ I find it difficult to explain to others things that I understand easily, when they don't understand it first time.}				
#' \item{\code{q6}}{ I really enjoy caring for other people.}
#' \item{\code{q8}}{ I find it hard to know what to do in a social situation.}				
#' \item{\code{q10}}{ People often tell me that I went too far in driving my point home in a discussion.}		
#' \item{\code{q11}}{ It doesn't bother me too much if I am late meeting a friend.}				
#' \item{\code{q12}}{ Friendships and relationships are just too difficult, so I tend not to bother with them.}	
#' \item{\code{q14}}{ I often find it difficult to judge if something is rude or polite.} 				
#' \item{\code{q15}}{ In a conversation, I tend to focus on my own thoughts rather than on what my listener might be thinking.}			
#' \item{\code{q18}}{ When I was a child, I enjoyed cutting up worms to see what would happen.}
#' \item{\code{q19}}{ I can pick up quickly if someone says one thing but means another.}				
#' \item{\code{q21}}{ It is hard for me to see why some things upset people so much.}				
#' \item{\code{q22}}{ I find it easy to put myself in somebody else's shoes.}				
#' \item{\code{q25}}{ I am good at predicting how someone will feel.}		
#' \item{\code{q26}}{ I am quick to spot when someone in a group is feeling awkward or uncomfortable.}				
#' \item{\code{q27}}{ If I say something that someone else is offended by, I think that that's their problem, not mine.}				
#' \item{\code{q28}}{ If anyone asked me if I liked their haircut, I would reply truthfully, even if I didn't like it.}				
#' \item{\code{q29}}{ I can't always see why someone should have felt offended by a remark.}				
#' \item{\code{q32}}{ Seeing people cry doesn't really upset me.}				
#' \item{\code{q34}}{ I am very blunt, which some people take to be rudeness, even though this is unintentional.}				
#' \item{\code{q35}}{ I don't tend to find social situations confusing.}				
#' \item{\code{q36}}{ Other people tell me I am good at understanding how they are feeling and what they are thinking.}				
#' \item{\code{q37}}{ When I talk to people, I tend to talk about their experiences rather than my own.}				
#' \item{\code{q38}}{ It upsets me to see an animal in pain.}				
#' \item{\code{q39}}{ I am able to make decisions without being influenced by people's feelings.}				
#' \item{\code{q41}}{ I can easily tell if someone else is interested or bored with what I am saying.}				
#' \item{\code{q42}}{ I get upset if I see people suffering on news programs.}				
#' \item{\code{q43}}{ Friends usually talk to me about their problems as they say that I am very understanding.}				
#' \item{\code{q44}}{ I can sense if I am intruding, even if the other person doesn't tell me.}				
#' \item{\code{q46}}{ People sometimes tell me that I have gone too far with teasing.}				
#' \item{\code{q48}}{ Other people often say that I am insensitive, though I don't always see why.}				
#' \item{\code{q49}}{ If I see a stranger in a group, I think that it is up to them to make an effort to join in.}				
#' \item{\code{q50}}{ I usually stay emotionally detached when watching a film.}				
#' \item{\code{q52}}{ I can tune into how someone else feels rapidly and intuitively.}				
#' \item{\code{q54}}{ I can easily work out what another person might want to talk about.}				
#' \item{\code{q55}}{ I can tell if someone is masking their true emotion.}				
#' \item{\code{q57}}{ I don't consciously work out the rules of social situations.}				
#' \item{\code{q58}}{ I am good at predicting what someone will do.}				
#' \item{\code{q59}}{ I tend to get emotionally involved with a friend's problems.}				
#' \item{\code{q60}}{ I can usually appreciate the other person's viewpoint, even if I don't agree with it.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#'
#' @references 
#' Baron-Cohen, Simon, and Sally Wheelwright. "The empathy quotient: an investigation of adults
#'  with Asperger syndrome or high functioning autism, and normal sex differences." Journal of autism
#'   and developmental disorders 34, no. 2 (2004): 163-175.
#'   
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#'
#' @docType data
#' @name empathy_cat
#' @usage data(empathy_cat)
"empathy_cat"






#' Systemizing Quotient Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 40-item Systemizing Quotient personality inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  13,256 response profiles collected by the Open Source Psychometrics Project in 2012,
#'   3050 response profiles
#' collected by Qualtrics in June 2018, and 1500 response profiles collected by YouGov in June 2018.
#' The sample from the Open Source Psychometrics Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.033) and a standard deviation (1.4)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are 1=Strongly disagree; 2=Slightly disagree; 3=Slightly agree; 4=Strongly agree
#' 
#' The wording of the question items:
#' 
#' \describe{
#' \item{\code{q1}}{   When I listen to a piece of music, I always notice the way it's structured.}
#' \item{\code{q4}}{   I prefer to read non-fiction than fiction.}
#' \item{\code{q5}}{    If I were buying a car, I would want to obtain specific information about its engine capacity.}
#' \item{\code{q6}}{   When I look at a painting, I do not usually think about the technique involved in making it.}
#' \item{\code{q7}}{    If there was a problem with the electrical wiring in my home, I'd be able to fix it myself.}
#' \item{\code{q11}}{    I rarely read articles or webpages about new technology.}
#' \item{\code{q12}}{   I do not enjoy games that involve a high degree of strategy.}
#' \item{\code{q13}}{    I am fascinated by how machines work.}
#' \item{\code{q15}}{    In math, I am intrigued by the rules and patterns governing numbers.}
#' \item{\code{q18}}{    I find it difficult to understand instruction manuals for putting appliances together.}
#' \item{\code{q19}}{    When I look at an animal, I like to know the precise species it belongs to.}
#' \item{\code{q20}}{    If I were buying a computer, I would want to know exact details about its hard drive capacity and processor speed.}
#' \item{\code{q23}}{    When I cook, I do not think about exactly how different methods and ingredients contribute to the final product.}
#' \item{\code{q24}}{    I find it difficult to read and understand maps.}
#' \item{\code{q25}}{   If I had a collection (e.g. CDs, coins, stamps), it would be highly organized.}
#' \item{\code{q26}}{    When I look at a piece of furniture, I do not notice the details of how it was constructed.}
#' \item{\code{q28}}{    When I learn about historical events, I do not focus on exact dates.}
#' \item{\code{q29}}{   When I read the newspaper, I am drawn to tables of information, such as football league scores or stock market indices.}
#' \item{\code{q30}}{   When I learn a language, I become intrigued by its grammatical rules.}
#' \item{\code{q31}}{   I find it difficult to learn my way around a new city.}
#' \item{\code{q32}}{   I do not tend to watch science documentaries on television or read articles about science and nature.}
#' \item{\code{q33}}{   If I were buying a stereo, I would want to know about its precise technical features.}
#' \item{\code{q34}}{    I find it easy to grasp exactly how odds work in betting.}
#' \item{\code{q35}}{   I am not very meticulous when I carry out D.I.Y.}
#' \item{\code{q37}}{   When I look at a building, I am curious about the precise way it was constructed.}
#' \item{\code{q38}}{    When an election is being held, I am not interested in the results for each constituency.}
#' \item{\code{q40}}{    I find it difficult to understand information the bank sends me on different investment and saving systems.}
#' \item{\code{q41}}{    When traveling by train, I often wonder exactly how the rail networks are coordinated.}
#' \item{\code{q42}}{    When I buy a new appliance, I do not read the instruction manual very thoroughly.}
#' \item{\code{q43}}{    If I were buying a camera, I would not look carefully into the quality of the lens.}
#' \item{\code{q44}}{    When I read something, I always notice whether it is grammatically correct.}
#' \item{\code{q45}}{   When I hear the weather forecast, I am not very interested in the meteorological patterns.}
#' \item{\code{q48}}{    When I look at a mountain, I think about how precisely it was formed.}
#' \item{\code{q49}}{    I can easily visualize how the highways in my region link up.}
#' \item{\code{q51}}{    When I'm in a plane, I do not think about the aerodynamics.}
#' \item{\code{q53}}{    When I am walking in the country, I am curious about how the various kinds of trees differ.}
#' \item{\code{q55}}{    I am interested in knowing the path a river takes from its source to the sea.}
#' \item{\code{q56}}{    I do not read legal documents very carefully.}
#' \item{\code{q57}}{   I am not interested in understanding how wireless communication works.}
#' \item{\code{q60}}{    I do not care to know the names of the plants I see.}
#' }
#' 
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' Baron-Cohen, Simon, Jennifer Richler, Dheraj Bisarya, Nhishanth Gurunathan,
#' and Sally Wheelwright.  "The systemizing quotient: an investigation of adults with
#' Asperger syndrome or high-functioning autism, and normal sex differences."
#' Philosophical Transactions of the Royal Society of London.  Series B: Biological Sciences
#' 358, no. 1430 (2003): 361-374.
#' 
#' Open Source Psychometrics Project. \url{https://openpsychometrics.org/_rawdata/}
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://openpsychometrics.org/_rawdata/}, maintained by Eric Jorgenson, for the raw Open Source Psychometrics Project site data.
#'
#' @docType data
#' @name systemizing_cat
#' @usage data(systemizing_cat)
"systemizing_cat"



#' Openness to Change (Schwartz Values) Cat Object
#' 
#' Cat model created for the  Openness to Change (Schwartz Values) inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  8,448 response profiles by myPersonality Project and 718 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-3.54) and a standard deviation (1.2)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options for the Schwartz Values survey are:
#' 
#' 0 - means the value is not at all important, it is not relevant as a guiding principle for you.
#' 3 - means the value is important.
#' 6 - means the value is very important.
#' 
#' -1 is for rating any values opposed to the principles that guide you.
#' 7 is for rating a value of supreme importance as a guiding principle in your life; ordinarily there are no more than two such values.
#' 
#' However, for computerized adaptive testing methods, the 0-7 response option range as been shifted to a 1-9 range. 
#' 
#' The wording of the question items: 
#' 
#' \describe{
#' \item{\code{q5}}{   FREEDOM (freedom of action and thought)}
#' \item{\code{q15}}{   RECIPROCATION OF FAVORS (avoidance of indebtedness)}
#' \item{\code{q31}}{   INDEPENDENT (self-reliant, self-sufficient)}
#' \item{\code{q41}}{  CHOOSING OWN GOALS (selecting own purposes)}
#' \item{\code{q53}}{    CURIOUS (interested in everything, exploring)}
#' \item{\code{q9}}{    AN EXCITING LIFE (stimulating experiences)}
#' \item{\code{q25}}{  A VARIED LIFE (filled with challenge, novelty and change)}
#' \item{\code{q37}}{   DARING (seeking adventure, risk)}
#' \item{\code{q4}}{   PLEASURE (gratification of desires)}
#' \item{\code{q50}}{  ENJOYING LIFE (enjoying food, sex, leisure, etc.)}
#' \item{\code{q57}}{ SELF-INDULGENT (doing pleasant things)}
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#' 
#' Schwartz, Shalom H. "Universals in the content and structure of values: 
#' Theoretical advances and empirical tests in 20 countries." 
#' Advances in experimental social psychology. Vol. 25. Academic Press, 1992. 1-65.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name sv_open_cat
#' @usage data(sv_open_cat)
"sv_open_cat"




#' Self-Transcendence (Schwartz Values) Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the Self-Transcendence (Schwartz Values) inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with 8,415 response profiles by myPersonalit Project and 718 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.004) and a standard deviation (1.2)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options for the Schwartz Values survey are:
#' 
#' 0 - means the value is not at all important, it is not relevant as a guiding principle for you.
#' 3 - means the value is important.
#' 6 - means the value is very important.
#' 
#' -1 is for rating any values opposed to the principles that guide you.
#' 7 is for rating a value of supreme importance as a guiding principle in your life; ordinarily there are no more than two such values.
#' 
#' However, for computerized adaptive testing methods, the 0-7 response option range as been shifted to a 1-9 range. 
#' 
#' The wording of the question items: 
#' 
#' \describe{
#' \item{\code{q1}}{   EQUALITY (equal opportunity for all)}
#' \item{\code{q17}}{   A WORLD AT PEACE (free of war and conflict)}
#' \item{\code{q24}}{  UNITY WITH NATURE (fitting into nature)}
#' \item{\code{q26}}{  WISDOM (a mature understanding of life)}
#' \item{\code{q29}}{    A WORLD OF BEAUTY (beauty of nature and the arts)}
#' \item{\code{q30}}{   SOCIAL JUSTICE (correcting injustice, care for the weak)}
#' \item{\code{q35}}{  BROADMINDED (tolerant of different ideas and beliefs)}
#' \item{\code{q38}}{  PROTECTING THE ENVIRONMENT (preserving nature)}
#' \item{\code{q33}}{   LOYAL (faithful to my friends, group)}
#' \item{\code{q45}}{  HONEST (genuine, sincere)}
#' \item{\code{q49}}{HELPFUL (working for the welfare of others)}
#' \item{\code{q52}}{ RESPONSIBLE (dependable, reliable)}
#' \item{\code{q54}}{ FORGIVING (willing to pardon others)}
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#' 
#' Schwartz, Shalom H. "Universals in the content and structure of values: 
#' Theoretical advances and empirical tests in 20 countries." 
#' Advances in experimental social psychology. Vol. 25. Academic Press, 1992. 1-65.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name sv_selftransc_cat
#' @usage data(sv_selftransc_cat)
"sv_selftransc_cat"




#' Self-Enhancement (Schwartz Values) Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the Self-Enhancement(Schwartz Values) inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with 8,497 response profiles by myPersonality Project and 718 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (-.09) and a standard deviation (1.2)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options for the Schwartz Values survey are:
#' 
#' 0 - means the value is not at all important, it is not relevant as a guiding principle for you.
#' 3 - means the value is important.
#' 6 - means the value is very important.
#' 
#' -1 is for rating any values opposed to the principles that guide you.
#' 7 is for rating a value of supreme importance as a guiding principle in your life; ordinarily there are no more than two such values.
#' 
#' However, for computerized adaptive testing methods, the 0-7 response option range as been shifted to a 1-9 range. 
#' 
#' The wording of the question items: 
#' 
#' \describe{
#' \item{\code{q4}}{   PLEASURE (gratification of desires)}
#' \item{\code{q50}}{   ENJOYING LIFE (enjoying food, sex, leisure, etc.)}
#' \item{\code{q57}}{  SELF-INDULGENT (doing pleasant things)}
#' \item{\code{q34}}{ AMBITIOUS (hard-working, aspiring)}
#' \item{\code{q39}}{   INFLUENTIAL (having an impact on people and events)}
#' \item{\code{q43}}{  CAPABLE (competent, effective, efficient)}
#' \item{\code{q55}}{  SUCCESSFUL (achieving goals)}
#' \item{\code{q3}}{ SOCIAL POWER (control over others, dominance)}
#' \item{\code{q12}}{  WEALTH (material possessions, money)}
#' \item{\code{q27}}{ AUTHORITY (the right to lead or command)}
#' \item{\code{q46}}{PRESERVING MY PUBLIC IMAGE (protecting my "face")}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#' 
#'
#' @references 
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#' 
#' Schwartz, Shalom H. "Universals in the content and structure of values: 
#' Theoretical advances and empirical tests in 20 countries." 
#' Advances in experimental social psychology. Vol. 25. Academic Press, 1992. 1-65.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name sv_selfenhance_cat
#' @usage data(sv_selfenhance_cat)
"sv_selfenhance_cat"






#' Conservation (Schwartz Values) Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the Conservation (Schwartz Values) inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  8,173 response profiles by the myPersonality Project and 718 response profiles
#' collected by YouGov in June 2018.  The sample from myPersonality Project was a convenience sample, and the
#' respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.480) and a standard deviation (1.4)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options for the Schwartz Values survey are:
#' 
#' 0 - means the value is not at all important, it is not relevant as a guiding principle for you.
#' 3 - means the value is important.
#' 6 - means the value is very important.
#' 
#' -1 is for rating any values opposed to the principles that guide you.
#' 7 is for rating a value of supreme importance as a guiding principle in your life; ordinarily there are no more than two such values.
#' 
#' However, for computerized adaptive testing methods, the 0-7 response option range as been shifted to a 1-9 range. 
#' 
#' The wording of the question items: 
#' 
#' \describe{
#' \item{\code{q11}}{   POLITENESS (courtesy, good manners)                          }
#' \item{\code{q20}}{  SELF-DISCIPLINE (self-restraint, resistance to temptation)   }
#' \item{\code{q40}}{  HONORING OF PARENTS AND ELDERS (showing respect)                 }
#' \item{\code{q47}}{ OBEDIENT (dutiful, meeting obligations)              }
#' \item{\code{q18}}{  RESPECT FOR TRADITION (preservation of time-honored customs) }
#' \item{\code{q32}}{ MODERATE (avoiding extremes of feeling & action)              }
#' \item{\code{q36}}{  HUMBLE (modest, self-effacing)                                    }
#' \item{\code{q44}}{ ACCEPTING MY PORTION IN LIFE (submitting to life's circumstances)}
#' \item{\code{q51}}{  DEVOUT (holding to religious faith & belief)                     }
#' \item{\code{q8}}{ SOCIAL ORDER (stability of society)                          }
#' \item{\code{q13}}{NATIONAL SECURITY (protection of my nation from enemies)     }
#' \item{\code{q15}}{RECIPROCATION OF FAVORS (avoidance of indebtedness)          }
#' \item{\code{q22}}{FAMILY SECURITY (safety for loved ones)                      }
#' \item{\code{q56}}{CLEAN (neat, tidy)                                   }
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' Stillwell, David, and Michal Kosinski. 2007. "myPersonality Project."
#' \url{https://sites.google.com/michalkosinski.com/mypersonality}
#' 
#' Schwartz, Shalom H. "Universals in the content and structure of values: 
#' Theoretical advances and empirical tests in 20 countries." 
#' Advances in experimental social psychology. Vol. 25. Academic Press, 1992. 1-65.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name sv_conservation_cat
#' @usage data(sv_conservation_cat)
"sv_conservation_cat"






#' Right Wing Authoritarianism Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20 item Right Wing Authoritarianism inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#'  2056 response profiles from Lucid, 2519 response profiles from MTurk,
#' and 1423 response profiles collected by YouGov in June 2018.  The sample from MTurk was a convenience sample
#' The respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.241) and a standard deviation (1)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 
#' 5 = Strongly agree, 4 = Somewhat agree, 3 = Neither agree nor disagree, 2 = Somewhat disagree, 1 = Strongly disagree
#' 
#' The wording of the question items is: 
#' 
#' \describe{
#' \item{\code{q1}}{   Our country desperately needs a mighty leader who will do what has to be done to destroy the radical new ways and sinfulness that are ruining us.}
#' \item{\code{q2}}{   Gays and lesbians are just as healthy and moral as anybody else.}
#' \item{\code{q3}}{   It is always better to trust the judgment of the proper authorities in government and religion than to listen to the noisy rabble-rousers in our society who are trying to create doubt in people's minds.}
#' \item{\code{q4}}{   Atheists and others who have rebelled against the established religions are no doubt every bit as good and virtuous as those who attend church regularly.}
#' \item{\code{q5}}{   The only way our country can get through the crisis ahead is to get back to our traditional values, put some tough leaders in power, and silence the troublemakers spreading bad ideas.}
#' \item{\code{q6}}{   There is absolutely nothing wrong with nudist camps.}
#' \item{\code{q7}}{   Our country needs free thinkers who have the courage to defy traditional ways, even if this upsets many people.}
#' \item{\code{q8}}{   Our country will be destroyed someday if we do not smash the perversions eating away at our moral fiber and traditional beliefs.}
#' \item{\code{q9}}{   Everyone should have their own lifestyle, religious beliefs, and sexual preferences, even if it makes them different from everyone else.}
#' \item{\code{q10}}{   The "old-fashioned ways" and the "old-fashioned values" still show the best way to live.}
#' \item{\code{q11}}{   You have to admire those who challenged the law and the majority's view by protesting for women's abortion rights, for animal rights, or to abolish school prayer.}
#' \item{\code{q12}}{   What our country really needs is a strong, determined leader who will crush evil, and take us back to our true path.}
#' \item{\code{q13}}{   Some of the best people in our country are those who are challenging our government, criticizing religion, and ignoring the "normal way things are supposed to be done."}
#' \item{\code{q14}}{   God's laws about abortion, pornography and marriage must be strictly followed before it is too late, and those who break them must be strongly punished.}
#' \item{\code{q15}}{   There are many radical, immoral people in our country today, who are trying to ruin it for their own godless purposes, whom the authorities should put out of action.}
#' \item{\code{q16}}{    A "woman's place" should be wherever she wants to be. The days when women are submissive to their husbands and social conventions belong strictly in the past.}
#' \item{\code{q17}}{    Our country will be great if we honor the ways of our forefathers, do what the authorities tell us to do, and get rid of the "rotten apples" who are ruining everything.}
#' \item{\code{q18}}{    There is no "ONE right way" to live life; everybody has to create their own way.}
#' \item{\code{q19}}{    Homosexuals and feminists should be praised for being brave enough to defy "traditional family values."}
#' \item{\code{q20}}{    This country would work a lot better if certain groups of troublemakers would just shut up and accept their group's traditional place in society.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}}
#'
#' @references 
#' 
#' Altemeyer, Bob. Enemies of freedom: Understanding right-wing authoritarianism. Jossey-Bass, 1988.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name rwa_cat
#' @usage data(rwa_cat)
"rwa_cat"







#' Social Dominance Orientation Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 15 item Social Dominance Orientation inventory
#' 
#' @details
#' Cat object containing item parameters for graded response model fit with
#' 2042 response profiles from Lucid, 1192 response profiles from MTurk,
#' and 1482 response profiles collected by YouGov in June 2018.  The sample from MTurk was a convenience sample
#' The respondents from YouGov were matched to a sampling frame on gender, age, race, and education.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the YouGov sample (.19) and a standard deviation (1)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 
#' 5 = Strongly agree, 4 = Somewhat agree, 3 = Neither agree nor disagree, 2 = Somewhat disagree, 1 = Strongly disagree
#' 
#' The wording of the question items is: 
#' 
#' \describe{
#' \item{\code{q1}}{    Some groups of people are simply inferior to other groups.}
#' \item{\code{q2}}{    In getting what you want, it is sometimes necessary to use force against other groups.}
#' \item{\code{q3}}{    It's OK if some groups have more of a chance in life than others.}
#' \item{\code{q4}}{    To get ahead in life, it is sometimes necessary to step on other groups.}
#' \item{\code{q5}}{    If certain groups stayed in their place, we would have fewer problems.}
#' \item{\code{q6}}{    It's probably a good thing that certain groups are at the top and other groups are at the bottom.}
#' \item{\code{q7}}{    Inferior groups should stay in their place.}
#' \item{\code{q8}}{    Sometimes other groups must be kept in their place.}
#' \item{\code{q9}}{    It would be good if groups could be equal.}
#' \item{\code{q10}}{    Group equality should be our ideal.}
#' \item{\code{q11}}{    All groups should be given an equal chance in life.}
#' \item{\code{q12}}{    We should do what we can to equalize conditions for different groups.}
#' \item{\code{q13}}{    Increased social equality is beneficial to society.}
#' \item{\code{q14}}{    We would have fewer problems if we treated people more equally.}
#' \item{\code{q15}}{    We should strive to make incomes as equal as possible.}
#' \item{\code{q16}}{    No group should dominate in society.}
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' 
#' Pratto, F., J. Sidanius, L. M. Stallworth, and B. F. Malle (1994), "Social Dominance Orientation: A
#' Personality Variable Predicting Social and Political Attitudes," Journal of Personality and
#' Social Psychology, 67, 741.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#'
#' @docType data
#' @name sdo_cat
#' @usage data(sdo_cat)
"sdo_cat"



#' Need for Affect Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 26 item Need for Affect inventory
#' 
#' @details
#' 
#' Cat object containing item parameters for graded response model fit with 2507 response profiles from MTurk,
#' 4990 response profiles collected by Qualtrics in June 2018,
#' and 1512 response profiles from The American Panel Survey (TAPS) in December 2014.
#' TAPS is a monthly online panel survey  of about 2,000 adults in the United States. The panel was recruited in 
#' the fall of 2011 using an address-based sampling frame.  TAPS surveys are administered online.  
#' Selected panelists who do not have a computer or online service are provided a computer and internet access by TAPS. 
#' 
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the TAPS sample (.236) and a standard deviation (1.3)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 
#' 5 = Strongly agree, 4 = Somewhat agree, 3 = Neither agree nor disagree, 2 = Somewhat disagree, 1 = Strongly disagree
#' 
#' The wording of the question items is: 
#' 
#' \describe{
#' \item{\code{q1}}{It is important for me to be in touch with my feelings.}
#' \item{\code{q2}}{I think that it is very important to explore my feelings.} 
#' \item{\code{q3}}{I am a very emotional person.} 
#' \item{\code{q4}}{It is important for me to know how others are feeling.} 
#' \item{\code{q5}}{Emotions help people get along in life.}
#' \item{\code{q6}}{Strong emotions are generally beneficial.} 
#' \item{\code{q7}}{I feel that I need to experience strong emotions regularly.}
#' \item{\code{q8}}{I approach situations in which I expect to experience strong emotions.} 
#' \item{\code{q9}}{I feel like I need a good cry every now and then.} 
#' \item{\code{q10}}{I like to dwell on my emotions.} 
#' \item{\code{q11}}{We should indulge our emotions.}
#' \item{\code{q12}}{I like decorating my bedroom with a lot of pictures and posters of things emotionally significant to me.} 
#' \item{\code{q13}}{The experience of emotions promotes human survival.} 
#' \item{\code{q14}}{I do not know how to handle my emotion, so I avoid them.} 
#' \item{\code{q15}}{I find strong emotions overwhelming and therefore try to avoid them.} 
#' \item{\code{q16}}{Emotions are dangerous---they tend to get me into situations that I would rather avoid.} 
#' \item{\code{q17}}{I would prefer not to experience either the lows or highs of emotion.}
#' \item{\code{q18}}{If I reflect on my past, I see that I tend to be afraid of emotions.} 
#' \item{\code{q19}}{I would love to be like Mr. Spock, who is totally logical and experiences little emotion.} 
#' \item{\code{q20}}{I have trouble telling the people close to me that I love them.} 
#' \item{\code{q21}}{Displays of emotions are embarrassing.} 
#' \item{\code{q22}}{Acting on ones emotions is always a mistake.} 
#' \item{\code{q23}}{I am sometimes afraid of how I might act if I become too emotional.} 
#' \item{\code{q24}}{Avoiding emotional events helps me sleep better at night.} 
#' \item{\code{q25}}{I wish I could feel less emotion.}
#' \item{\code{q26}}{People can function most effectively when they are not experiencing strong emotions.} 
#' }
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' 
#' Maio, Gregory R., and Victoria M. Esses. "The need for affect: Individual
#'  differences in the motivation to approach or avoid emotions." 
#'  Journal of personality 69.4 (2001): 583-614.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://wc.wustl.edu/american-panel-survey} for raw TAPS data.
#'
#' @docType data
#' @name nfa_cat
#' @usage data(nfa_cat)
"nfa_cat"





#' Need to Evaluate Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 16 item Need to Evaluate inventory
#' 
#' @details
#'  Cat object containing item parameters for graded response model fit with
#'   2534 response profiles from MTurk,
#' 4993 response profiles collected by Qualtrics in June 2018,
#' and 1512 response profiles from The American Panel Survey (TAPS) in December 2014.
#' TAPS is a monthly online panel survey  of about 2,000 adults in the United States.
#' The panel was recruited in the fall of 2011 using an address-based sampling frame.
#' TAPS surveys are administered online.  Selected panelists who do not have a computer or
#'  online service are provided a computer and internet access by TAPS. 
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the TAPS sample (.002) and a standard deviation (1.3)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 
#' 5 = Extremely characteristic of me, 4 = Somewhat characteristic of me, 3 = Uncertain, 2 = Somewhat uncharacteristic of me, 1 = Extremely uncharacteristic of me
#' 
#' The wording of the question items is: 
#' 
#' \describe{
#' \item{\code{q1}}{ I form opinions about everything.} 
#' \item{\code{q2}}{ I prefer to avoid taking extreme positions.} 
#' \item{\code{q3}}{ It is very important to me to hold strong opinions.} 
#' \item{\code{q4}}{ I want to know exactly what is good and bad about everything.} 
#' \item{\code{q5}}{ I often prefer to remain neutral about complex issues.} 
#' \item{\code{q6}}{ If something does not affect me, I do not usually determine if it is good or bad.} 
#' \item{\code{q7}}{ I enjoy strongly liking and disliking things.}
#' \item{\code{q8}}{ There are many things for which I do not have a preference.} 
#' \item{\code{q9}}{ It bothers me to remain neutral.} 
#' \item{\code{q10}}{ I like to have strong opinions even when I am not personally involved.} 
#' \item{\code{q11}}{ I have many more opinions than the average person.}
#' \item{\code{q12}}{ I would rather have a strong opinion than no opinion at all.} 
#' \item{\code{q13}}{ I pay a lot of attention to whether things are good or bad.} 
#' \item{\code{q14}}{ I only form strong opinions when I have to.} 
#' \item{\code{q15}}{ I like to decide that new things are really good or really bad.}
#' \item{\code{q16}}{ I am pretty much indifferent to many important issues.} 
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' 
#' 
#' Jarvis, W. Blair G., and Richard E. Petty.
#' "The need to evaluate." Journal of personality and social psychology 70.1 (1996): 172.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://wc.wustl.edu/american-panel-survey} for raw TAPS data.
#'
#' @docType data
#' @name nte_cat
#' @usage data(nte_cat)
"nte_cat"




#' Need for Cognition Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 34 item Need for Cognition inventory
#' 
#' @details
#'  Cat object containing item parameters for graded response model fit with 4985 response profiles collected by Qualtrics in June 2018
#' and 1512 response profiles from The American Panel Survey (TAPS) in December 2014.
#' TAPS is a monthly online panel survey  of about 2,000 adults in the United States. The panel was recruited in the fall of 2011 using an address-based sampling frame.  TAPS surveys are administered online.  Selected panelists who do not have a computer or online service are provided a computer and internet access by TAPS. 
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the TAPS sample (.003) and a standard deviation (1.6)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 
#' 5 = Strongly agree, 4 = Somewhat agree, 3 = Neither agree nor disagree, 2 = Somewhat disagree, 1 = Strongly disagree
#' 
#' The wording of the question items is: 
#' 
#' \describe{
#' \item{\code{q1}}{	I would prefer complex to simple problems.}  
#' \item{\code{q2}}{	I don't like to have the responsibility for handling a situation that requires a lot of thinking.}
#' \item{\code{q3}}{	Thinking is not my idea of fun.} 
#' \item{\code{q4}}{    	I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.} 
#' \item{\code{q5}}{	I try to anticipate and avoid situations where there is a likely chance I will have to think in depth about something.} 
#' \item{\code{q6}}{	 	I find little satisfaction in deliberating hard and for long hours.}
#' \item{\code{q7}}{	 	I only think as hard as I have to.}
#' \item{\code{q8}}{	 	I prefer to think about small, daily projects more than long-term ones.} 
#' \item{\code{q9}}{	 	I like tasks that require little thought once I've learned them.}  
#' \item{\code{q10}}{	 	The idea of relying on thought to make my way to the top does not appeal to me.}
#' \item{\code{q11}}{	 	I really enjoy a task that involves coming up with new solutions to problems.}
#' \item{\code{q12}}{	 	Learning new ways to think doesn't excite me very much.}  
#' \item{\code{q13}}{	I prefer my life to be filled with puzzles that I must solve.}  
#' \item{\code{q14}}{	 	The notion of thinking abstractly is not appealing to me.}
#' \item{\code{q15}}{	 	I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.} 
#' \item{\code{q16}}{	I feel relief rather the satisfaction after completing a task that required a lot of mental effort.}
#' \item{\code{q17}}{	Its enough for me that something gets the job done, I don't care how or why it works.} 
#' \item{\code{q18}}{	 	I usually end up deliberating about issues even when they do not affect me personally.}  
#' \item{\code{q19}}{	 	I tend to set goals that can be accomplished only by expending considerable mental effort.} 
#' \item{\code{q20}}{  	I am usually tempted to put more thought into a task than the job minimally requires.} 
#' \item{\code{q21}}{  	I am hesitant about making important decisions after thinking about them.} 
#' \item{\code{q22}}{  	I prefer just to let things happen rather than try to understand why they turned out that way.} 
#' \item{\code{q23}}{  	I have difficulty thinking in new and unfamiliar situations.} 
#' \item{\code{q24}}{  	I am an intellectual.} 
#' \item{\code{q25}}{  	I don't reason well under pressure.} 
#' \item{\code{q26}}{  	I more often talk with other people about the reasons/possible solutions to international problems than about gossip or tidbits about what famous people are doing.} 
#' \item{\code{q27}}{ These days, I see little chance for performing well, even in intellectual jobs, unless one knows the right people.}
#' \item{\code{q28}}{ More often than not, more thinking just leads to more errors.} 
#' \item{\code{q29}}{ I appreciate opportunities to discover the strengths and weaknesses of my own reasoning.} 
#' \item{\code{q30}}{ I prefer watching educational programs more than entertainment programs.} 
#' \item{\code{q31}}{ I think best when those around me are very intelligent.}
#' \item{\code{q32}}{ Simply knowing the answer rather than understanding the reasons for the answer to a problem is fine with me.} 
#' \item{\code{q33}}{ Ignorance is bliss.} 
#' \item{\code{q34}}{ I enjoy thinking about an issue even when the results of my thought will have no effect on the outcome of the issue.} 
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' 
#' Cacioppo, John T., and Richard E. Petty. "The need for cognition."
#'  Journal of personality and social psychology 42.1 (1982): 116.
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://wc.wustl.edu/american-panel-survey} for raw TAPS data.
#'
#' @docType data
#' @name nfc_cat
#' @usage data(nfc_cat)
"nfc_cat"







#' Narcissistic Personality Cat Object
#' 
#' Cat object containing item parameters for latent trait model fit with responses
#' to the  40 item Narcissistic Personality inventory
#' 
#' @details
#' Cat object containing item parameters for latent trait model fit with
#' 2945 response profiles collected by Qualtrics in June 2018
#' and 10440 response profiles collected by the Open Source Psychometrics Project in 2012.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{ltmCat}} for details regarding the latent trait model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the Qualtrics sample (-.069) and a standard deviation (1)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Item responses are paired. Respondents had to choose the one that fit them the best.
#' 
#' The wording of the item responses is: 
#' 
#' \describe{
#' \item{\code{Q1*}}{0 = I am not good at influencing people. 1 = I have a natural talent for influencing people.}
#' \item{\code{Q2}}{0 = Modesty doesn't become me. 1 = I am essentially a modest person.}
#' \item{\code{Q3}}{0 = I would do almost anything on a dare. 1 = I tend to be a fairly cautious person.}
#' \item{\code{Q4}}{0 = When people compliment me I sometimes get embarrassed. 1 = I know that I am good because everybody keeps telling me so.}
#' \item{\code{Q5}}{0 = The thought of ruling the world frightens the hell out of me. 1 = If I ruled the world it would be a better place.}
#' \item{\code{Q6}}{0 = I can usually talk my way out of anything. 1 = I try to accept the consequences of my behavior.}
#' \item{\code{Q7}}{0 = I prefer to blend in with the crowd. 1 = I like to be the center of attention.}
#' \item{\code{Q8}}{0 = I will be a success. 1 = I am not too concerned about success.}
#' \item{\code{Q9}}{0 = I am no better or worse than most people. 1 = I think I am a special person.}
#' \item{\code{Q10}}{0 = I am not sure if I would make a good leader. 1 = I see myself as a good leader.}
#' \item{\code{Q11}}{0 = I am assertive. 1 = I wish I were more assertive.}
#' \item{\code{Q12}}{0 = I like to have authority over other people. 1 = I don't mind following orders.}
#' \item{\code{Q13}}{0 = I find it easy to manipulate people. 1 = I don't like it when I find myself manipulating people.}
#' \item{\code{Q14}}{0 = I insist upon getting the respect that is due me. 1 = I usually get the respect that I deserve.}
#' \item{\code{Q15}}{0 = I don't particularly like to show off my body. 1 = I like to show off my body.}
#' \item{\code{Q16}}{0 = I can read people like a book. 1 = People are sometimes hard to understand.}
#' \item{\code{Q17}}{0 = If I feel competent I am willing to take responsibility for making decisions. 1 = I like to take responsibility for making decisions.}
#' \item{\code{Q18}}{0 = I just want to be reasonably happy. 1 = I want to amount to something in the eyes of the world.}
#' \item{\code{Q19}}{0 = My body is nothing special. 1 = I like to look at my body.}
#' \item{\code{Q20}}{0 = I try not to be a show off. 1 = I will usually show off if I get the chance.}
#' \item{\code{Q21}}{0 = I always know what I am doing. 1 = Sometimes I am not sure of what I am doing.}
#' \item{\code{Q22}}{0 = I sometimes depend on people to get things done. 1 = I rarely depend on anyone else to get things done.}
#' \item{\code{Q23}}{0 = Sometimes I tell good stories. 1 = Everybody likes to hear my stories.}
#' \item{\code{Q24}}{0 = I expect a great deal from other people. 1 = I like to do things for other people.}
#' \item{\code{Q25}}{0 = I will never be satisfied until I get all that I deserve. 1 = I take my satisfactions as they come.}
#' \item{\code{Q26}}{0 = Compliments embarrass me. 1 = I like to be complimented.}
#' \item{\code{Q27}}{0 = I have a strong will to power. 1 = Power for its own sake doesn't interest me.}
#' \item{\code{Q28}}{0 = I don't care about new fads and fashions. 1 = I like to start new fads and fashions.}
#' \item{\code{Q29}}{0 = I like to look at myself in the mirror. 1 = I am not particularly interested in looking at myself in the mirror.}
#' \item{\code{Q30}}{0 = I really like to be the center of attention. 1 = It makes me uncomfortable to be the center of attention.}
#' \item{\code{Q31}}{0 = I can live my life in any way I want to. 1 = People can't always live their lives in terms of what they want.}
#' \item{\code{Q32}}{0 = Being an authority doesn't mean that much to me. 1 = People always seem to recognize my authority.}
#' \item{\code{Q33}}{0 = I would prefer to be a leader. 1 = It makes little difference to me whether I am a leader or not.}
#' \item{\code{Q34}}{0 = I am going to be a great person. 1 = I hope I am going to be successful.}
#' \item{\code{Q35}}{0 = People sometimes believe what I tell them. 1 = I can make anybody believe anything I want them to.}
#' \item{\code{Q36}}{0 = I am a born leader. 1 = Leadership is a quality that takes a long time to develop.}
#' \item{\code{Q37}}{0 = I wish somebody would someday write my biography. 1 = I don't like people to pry into my life for any reason.}
#' \item{\code{Q38}}{0 = I get upset when people don't notice how I look when I go out in public. 1 = I don't mind blending into the crowd when I go out in public.}
#' \item{\code{Q39}}{0 = I am more capable than other people. 1 = There is a lot that I can learn from other people.}
#' \item{\code{Q40}}{0 = I am much like everybody else. 1 = I am an extraordinary person.}
#' }
#' 
#' *Note that Item 1 is "reverse coded" from the coding that is typically used.
#' This is because item parameter estimation constrains the first item's discrimination parameter
#' to be positive.  So the reverse coding was done for the more intuitive
#' interpretation of item parameters and theta
#' estimates as larger, positive values indicate "more" of the latent trait.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{ltmCat}} 
#'
#' @references 
#' 
#' Raskin, R., and H. Terry (1988), "A Principal-Components Analysis of the Narcissistic Personality
#' Inventory and Further Evidence of Its Construct Validity," Journal of Personality and Social
#' Psychology, 54, 890-902.
#' 
#' Open Source Psychometrics Project. \url{https://openpsychometrics.org/_rawdata/}
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://openpsychometrics.org/_rawdata/}, maintained by Eric Jorgenson, for the raw Open Source Psychometrics Project data.
#'
#' @docType data
#' @name npi_cat
#' @usage data(npi_cat)
"npi_cat"







#' Machiavellianism Personality Cat Object
#' 
#' Cat object containing item parameters for graded response model fit with responses to the 20 item machiavellianism inventory
#' 
#' @details
#'  Cat object containing item parameters for graded response model fit with
#'  3020 response profiles collected by Qualtrics in June 2018
#' and 12299 response profiles collected by the Open Source Psychometrics Project in 2012.
#' 
#' See \code{\link{Cat-class}} for details regarding the Cat object structure.
#' See \code{\link{grmCat}} for details regarding the graded response model.
#' 
#' A normal prior was chosen with mean from the theta estimates from the Qualtrics sample (-0.713) and a standard deviation (1)
#' wide enough to envelope the range of estimated theta values across both samples.
#' 
#' Note that due to how the item parameters were estimated, large, positive values indicate "more" of the latent trait.
#' 
#' Response options are:
#' 5 = Strongly agree, 4 = Somewhat agree, 3 = Neither agree nor disagree, 2 = Somewhat disagree, 1 = Strongly disagree
#' 
#' The wording of the item responses is: 
#' 
#' \describe{
#' \item{\code{Q1}}{ Never tell anyone the real reason you did something unless it is useful to do so.}
#' \item{\code{Q2}}{ The best way to handle people is to tell them what they want to hear.}
#' \item{\code{Q3}}{ One should take action only when sure it is morally right.}
#' \item{\code{Q4}}{ Most people are basically good and kind.}
#' \item{\code{Q5}}{ It is safest to assume that all people have a vicious streak and it will come out when they are given a chance.}
#' \item{\code{Q6}}{ Honesty is the best policy in all cases.}
#' \item{\code{Q7}}{ There is no excuse for lying to someone else.}
#' \item{\code{Q8}}{ Generally speaking, people won't work hard unless they're forced to do so.}
#' \item{\code{Q9}}{ All in all, it is better to be humble and honest than to be important and dishonest.}
#' \item{\code{Q10}}{ When you ask someone to do something for you, it is best to give the real reasons for wanting it rather than giving reasons which carry more weight.}
#' \item{\code{Q11}}{ Most people who get ahead in the world lead clean, moral lives.}
#' \item{\code{Q12}}{ Anyone who completely trusts anyone else is asking for trouble.}
#' \item{\code{Q13}}{ The biggest difference between most criminals and other people is that the criminals are stupid enough to get caught.}
#' \item{\code{Q14}}{ Most people are brave.}
#' \item{\code{Q15}}{ It is wise to flatter important people.}
#' \item{\code{Q16}}{ It is possible to be good in all respects.}
#' \item{\code{Q17}}{ P.T. Barnum was wrong when he said that there's a sucker born every minute.}
#' \item{\code{Q18}}{ It is hard to get ahead without cutting corners here and there.}
#' \item{\code{Q19}}{ People suffering from incurable diseases should have the choice of being put painlessly to death.}
#' \item{\code{Q20}}{ Most people forget more easily the death of their parents than the loss of their property.}
#' }
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{grmCat}} 
#'
#' @references 
#' 
#' Christie, R., F. L. Geis, and D. Berger (1970), Studies in Machiavellianism, New York: Academic Press.
#' 
#' Open Source Psychometrics Project. \url{https://openpsychometrics.org/_rawdata/}
#' 
#' @source 
#' 
#' See \url{https://dataverse.harvard.edu/dataverse/pdsl} for the raw YouGov and/or Qualtrics data.
#' 
#' See \url{https://openpsychometrics.org/_rawdata/}, maintained by Eric Jorgenson, for the raw Open Source Psychometrics Project data.
#'
#' @docType data
#' @name mach_cat
#' @usage data(mach_cat)
"mach_cat"



#' Example Qualtrics Data for Adaptive Inventory
#' 
#' Example data when including an adaptive inventory on a Qualtrics survey
#' 
#' @details
#'  This data set includes four example responses to a length four adaptive inventory
#'  for the agreeableness battery.
#' 
#' See \code{\link{agree_cat}} for details regarding the Cat object structure and battery items.
#' 
#' See \code{\link{readQualtrics}} for how to clean adaptive inventory response profiles.
#' 
#' @seealso \code{\link{agree_cat}}, \code{\link{readQualtrics}}
#'
#' @docType data
#' @name ex_qualtrics_results
#' @usage data(ex_qualtrics_results)
"ex_qualtrics_results"


#' Example Qualtrics Data for Multiple Adaptive Inventories
#' 
#' Example data when including multiple adaptive inventory on a Qualtrics survey
#' 
#' @details
#'  This data set includes four example responses to length four adaptive inventories
#'  for both the agreeableness and neuroticism batteries.
#' 
#' See \code{\link{agree_cat}} and \code{\link{neuro_cat}} for details regarding the Cat object structure and battery items.
#' 
#' See \code{\link{readQualtrics}} for how to clean adaptive inventory response profiles.
#' 
#' @seealso \code{\link{agree_cat}}, \code{\link{neuro_cat}},\code{\link{readQualtrics}}
#'
#' @docType data
#' @name ex_qualtrics_results_multiple
#' @usage data(ex_qualtrics_results_multiple)
"ex_qualtrics_results_multiple"

