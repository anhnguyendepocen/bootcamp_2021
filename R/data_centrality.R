#' Advice centrality dataset
#'
#' Dataset that is loosely based on the article (and findings therein)
#' "HOW DO THEY GET THERE? AN EXAMINATION OF THE ANTECEDENTS OF CENTRALITY
#' IN TEAM NETWORKS" by Katherine Klein et al., 2004, 
#' Academy of Management Journal', Vol. 47, No. 6, 952–963.
#' 
#' As in the original article, the aim is to model the centrality of 
#' individuals in an organizational network. Centrality is measured through 
#' indegree and is simply the count of individuals in a team who indicate 
#' that a focal individual is someone they go to for advice (advice centrality), 
#' someone they consider a friend (friendship centrality), 
#' or someone with whom they have a difficult relationship 
#' (adversarial centrality). So, higher centrality means that more people go 
#' the individual to get advice (advice network), consider that person a friend 
#' (friendship network), or consider that person to be difficult and 
#' unpleasant to get along with (adversarial network).
#' 
#' The explanatory variables are:
#' 
#' \describe{
#'   \item{education}{education on a six-point scale on which 1 was 
#'   “some high school (grades 9 –11)” and 6 was “graduate degree 
#'   (master’s, Ph.D., J.D., M.D., etc.).” }
#'   \item{race}{0 for white, 1 for non-white}
#'   \item{gender}{0 for male, 1 for female}
#'   \item{age}{age in years}
#'   \item{activity_preference}{Personality trait on a 5-point scale. 
#'   Individuals who are high in activity preference have a strong work 
#'   ethic and an ability to delay rewards. They dislike waste and prefer efficiency}
#'   \item{conscientiousness}{Personality trait on a 5-point scale. 
#'   Conscientiousness refers to the tendency to be dutiful, persistent, 
#'   responsible, careful, prepared, organized, and detail-oriented.}
#'   \item{extraversion}{Personality trait on a 5-point scale. 
#'   Extraversion refers to the tendency to be outgoing, gregarious, 
#'   energetic, assertive, active, and cheerful in outlook.}
#'   \item{neuroticism}{Personality trait on a 5-point scale. 
#'   Neuroticism is the tendency to be moody, anxious, depressed, insecure, 
#'   hostile, and/or irritable.}
#'   \item{agreeableness}{Personality trait on a 5-point scale. 
#'   Agreeableness describes the tendency to be cooperative, compliant, 
#'   sincere, gentle, and trusting.}
#'   \item{openness_experience}{Personality trait on a 5-point scale. 
#'   Openness to experience is the tendency to be imaginative, intellectual, 
#'   creative, open-minded, unconventional, nonconforming, and autonomous.}
#'   \item{gender_similarity}{The proportion of co-workers having the same gender 
#'   as the focal individual}
#'   \item{race_similarity}{The proportion of co-workers having the same race 
#'   as the focal individual}
#' }
#' 
#' The centrality scores were collected five months after the personality traits 
#' and demographics were collected. The idea is that personality at time 0 
#' requires some time to manifest network ties.
#' 
#' An important difference with the original dataset is that the current dataset 
#' is purely at the individual level, whereas the original study was a 
#' multilevel study with individuals nested in teams.
#' 
#' @name centrality
#' @docType data
#' @keywords datasets
NULL
