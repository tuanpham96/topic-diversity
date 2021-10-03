# Reviews from Complex Networks and Applications

> SUBMISSION: 242
> TITLE: Modelling the effects of self-learning and social influence on the diversity of knowledge

The revision version does not need a rebuttal letter, below are the comments from the reviewers, and just basically checklist of what to do with each, if any, and to document my thinking about the issues. This is **not** a rebuttal/response letter. Plus, given only a week to revise at the literal start of my academic quarter, I was only able to do so much unfortunately.

## REVIEW 1

> The paper presents a computational model of knowledge  acquisition through self-learning or social learning/influence.

> While the paper is interesting to read, the writing needs to be improved significantly so it is a research paper. The paper should not include expressions that identify the "I" and "author's wishes, such as "I present", "I want to examine", "I want to focus".  Please refer to standard scientific paper writing.

- Soln: Thank you for the suggestions, I adjusted my writing to address this.

> The introduction of the figures need to be stated clearly so we can follow them (what is it plotted, what is the x-axis, what is the y-axis, and the difference between the plots within each image).
> Good discussion of the results and their implications, but I am missing the point of the additional figures: better to include fewer and have a deeper discussion on them, then to put figures that are not meaningful without your discussion.

- Cmt: I included them for the sake of thoroughness. Many issues were due to the page limit and I wish I could expand further on many parts, mostly describing the figures clearer, as well as describing the intentions of the supplemental figures better. For example, the reason for nonblock interlayer initialization was because I believed it could affect the entropy of the topic distribution significantly: starting at well-connected topics would lead to many cases of being "stuck" there (the population starting in an island would have a hard time getting out).

> Also, what is the reason for bolding the names of the figures throughout the paper.

- Cmt: This is just a convention in my own field as I saw that styling a lot in biology papers/neuroscience papers.
- Soln: I unbolded the figure names in response to this.

## REVIEW 2

> Engaging research idea and a nice model to read about that could be of interest to a wide audience at the conference. The author investigates how an agent’s propensity to learn by self-exploration versus social influence affects the distribution of acquired knowledge. The model contains a network of knowledge topics and a social network of agents and the author investigates how the topology of these two, together with the parameter alpha contributes to knowledge diversity measured by a few individual and population-level metrics.

> The model is highly stylized but still offers some interesting insights and as such can be used as a fruitful playground for posing hypotheses and their investigation. Some model limitations that could be discussed in more detail are listed below:

- Cmt: These were all fantastic questions! I really hope I'll have the time to integrate these in the future soon.

> 1) The effects of network topology (or the lack of it) could be more discussed in the paper, e.g. Fig S1 shows that quite different networks qualitatively lead to the same knowledge which should be more stressed and discussed in the main text.

- Cmt: Yes, due to limited space I was not able to address some of the small differences between the different intralayer nonblock models. However, the results show that the underlying trend is pretty common across these different choices. The quantitative differences are harder to address due to the different properties between the models (like edge density).

> Also, the assumption that both the social network and topic network have the same topology is strong and could be more justified.

- Cmt: Yes, this was done for practical reasons, due to the overwhelming number of possibilities to consider if they are not the same. Currently I don't have the resources to run all of them.
- Soln: I addressed this issue in the discussion.

> 2) A few parameter values and assumptions could be a bit more discussed, for example, the number of agents/topics - e.g. why more topics than users? Wikipedia, which was mentioned as one motivating example in the paper’s abstract, in its English version has about 6 million articles, but way more (potential) readers.

- Cmt: Yes, this is correct. I initially assumed knowledge would eventually outgrow human population (but of course I will have to do projected growth of both Wikipedia and human population size to see if/when this could happen). At the same time, if we consider the size of the internet in general and consider the agents are just random walkers on the internet, I believe there are still interesting interpretations. But you are right to address the current mismatch of real size ratio to the model assumption.
- Soln: Future steps would require integrating the realistic size ratios to address this. I've addressed some of these points in the discussion.

> 3) Given the different network topologies investigated, it would be interesting to discuss possible relations between the agent’s position in the network and acquired knowledge.

- Cmt: If I understand this correctly, an example would be to analyze the relation between an agent's degrees or certain centrality measures (or whether an agent is a hub) in G_a and the acquired topic sets, possibly also compared to the diversity of knowledge as well as the same types of measures in G_t. If so, I agree. I did consider examining some version of this when I was brainstorming but didn't have time to integrate.
- Soln: I will include this suggestion (or at least how I interpreted it above) in the discussion. If R2 sees this comment and wants to correct me, just hit me up somehow (maybe open an issue with a dummy github acc? since apparently github doesn't do anonymous comments.)

> 4) Typo at the beginning of section 2.2 “The concept of knowledge diversity” is probably there by mistake.

- Soln: Yes, thanks for pointing that out. Removed.

## REVIEW 3

> The paper analyzes the change in the diversity of learned topics when self-learning (i.e., learn topics related to current knowledge) and social influence (i.e., learn topics known by peers) are considered.

> While the models are simple, the author considers several factors (such as intralayer random network models of scale-free, Erdos-Reyni, Watts-Strogatz, and Scholastic Block Models as well as interlayer initialization strategies for nonblock and block models) and performs an in-depth analysis of knowledge diversity in the population, individual, and group.

> The source code is shared through a public repository.

> Typo on page 4, Intralayer random models: Is "the model types and model hyper-parameters are" similar OR the same?

- Soln: Yes, thanks for pointing that out. It's "the same".

> Overall, the study is well prepared, performed, and presented.
