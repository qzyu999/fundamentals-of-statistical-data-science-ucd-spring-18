library(ggplot2); library(beepr); library(tidyverse); library(dplyr) # Load libraries
properties = read.csv("properties.csv"); colors = read.csv("color_combos.csv") # Load datasets

### Part 1

# 1

community_chest = function(current_position, current_deck) { # Community Chest
  if (current_position == 2 || current_position == 17 || current_position == 33) {
    current_deck_draw = current_deck[1] # Draw card
    if (current_deck_draw == 1) {
      current_position = 0 # sends player to GO
    } else if (current_deck_draw == 2) {
      current_position = 10 # sends player to Jail
    }
    
    current_deck = c(current_deck[-1], current_deck[1]) # Place card on the bottom
  }
  community_result = list(current_position, current_deck) # Return both the deck and position
  return(community_result)
} # Community Chest cards

chance_deck = function(current_position, current_deck) {
  if (current_position == 7 || current_position == 22 || current_position == 36) { # Chance
    current_deck_draw = current_deck[1] # Draw card
    if (current_deck_draw == 1) { # check if card drawn is GO or JAIL
      current_position = 0 # sends player to GO
    } else if (current_deck_draw == 2) {
      current_position = 10 # sends player to JAIL
    } else if (current_deck_draw == 3) {
      current_position = 11 # sends player to C1
    } else if (current_deck_draw == 4) {
      current_position = 24 # sends player to E3
    } else if (current_deck_draw == 5) {
      current_position = 39 # sends player to H2
    } else if (current_deck_draw == 6) {
      current_position = 5 # sends player to R1
    } else if (current_deck_draw == 7 | current_deck_draw == 8) { # Check Railroad
      index = current_position # Determine which Railroad to send the player to
      if (index < 5) {
        current_position = 5
      } else if (index < 15) {
        current_position = 15
      } else if (index < 25) {
        current_position = 25
      } else if (index < 35) {
        current_position = 35
      }
    } else if (current_deck_draw == 9) { # Check Utility
      index = current_position # Determine which Utility to send the player to
      if (index < 12) {
        current_position = 12
      } else {
        current_position = 28
      }
    } else if (current_deck_draw == 10) { # Check if go back 3 steps
      index = current_position # Determine whether the player needs to go back over GO
      if (index >= 3) {
        index = index - 3
        current_position = index
      } else if (index == 2) {
        current_position = 39
      } else if (index == 1) {
        current_position = 38
      } else {
        current_position = 37
      }
    }
    current_deck = c(current_deck[-1], current_deck[1]) # Place card on the bottom
  }
  chance_result = list(current_position, current_deck) # Return both the deck and position
  return(chance_result)
} # Chance Deck cards

triple = function(current_position, turn, dice_vector) {
  if (turn >= 3) { # check the dice_vector, a T/F vector
    if (dice_vector[turn] & dice_vector[turn-1] & dice_vector[turn-2]) {
      current_position = 10
    }
  } 
  return(current_position)
} # 3 Consecutive pairs

loop_39 = function(current_location) {
  return(current_location %% 40)
} # Loops at 39

jail_30 = function(current_position) {
  if (current_position == 30) {
    current_position = 10
  }
  return(current_position)
} # Sends player to jail if they land on 30

set.seed(141)
simulate_monopoly = function(n, d) {
  chance_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize chance deck
  chance <- sample(chance_cards) # Shuffle cards
  community_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize community chest deck
  community <- sample(community_cards) # Shuffle cards
  
  die1 = sample(1:d, n, replace = T) # Roll die 1
  die2 = sample(1:d, n, replace = T) # Roll die 2
  dice = die1 + die2 # Combine dice into vector
  dice_vec = die1 == die2 # Create T/F vector for triples
  player_positions = rep(0, n)

  current_position = 0 # Initialize current position of player
  for (i in 1:n) {
    current_position = dice[i] + current_position # Update current position
    current_position = loop_39(current_position) # Loops at position 39
    
    current_position = triple(current_position = current_position, # Check for three consecutive pairs
                              turn = i, dice_vector = dice_vec)
    
    chance_list = chance_deck(current_position = current_position, # Check for chance deck
                              current_deck = chance)
    current_position = chance_list[[1]]
    chance = chance_list[[2]]
    
    cc_list = community_chest(current_position = current_position, # Check for community chest
                              current_deck = community)
    current_position = cc_list[[1]]
    community = cc_list[[2]]
    
    current_position = jail_30(current_position = current_position) # Check if player is at 30
    
    player_positions[i] = current_position # Update player position tracker
  }
  player_positions = c(0, player_positions) # Include initial location 0
  return(factor(player_positions, 0:39)) # Return factorized version of positions
} # Simulate game of monopoly

# 2

estimate_monopoly = function(n, d) {
  k = 1000 # Number of simulations
  
  position_list = lapply(1:k, function(x) simulate_monopoly(n, d)) # Generate k simulations of monopoly
  position_ratio = table(unlist(position_list)) # Create a table of the vector
  position_prop = as.data.frame(prop.table(position_ratio)*100) # Generate proportion table of the results
  beep(2)
  return(position_prop)
} # Perform estimations of the simulation

# Simulate monopoly games for 3, 4, 5, and 6 sided die
die6 = estimate_monopoly(10000, 6); die5 = estimate_monopoly(10000, 5)
die4 = estimate_monopoly(10000, 4); die3 = estimate_monopoly(10000, 3)

# Add number of sides column
die6$Sides = 6; die5$Sides = 5; die4$Sides = 4; die3$Sides = 3

dice_sides = rbind(die3, die4, die5, die6) # Combine die into single data frame

colnames(dice_sides) = c("Die", "Percentage", "Sides") # Give appropriate labels

dice_sides = dice_sides %>% # Arrange data frame
  group_by(Sides) %>%
  arrange(-Percentage,.by_group = TRUE)

top3_positions = dice_sides %>% # Determine the top 3 positions per side
  group_by(Sides) %>%
  top_n(n = 3, wt = Percentage)

line_plot = ggplot(data = dice_sides, aes(x = Die, y = Percentage, # Create line plot of data
  group = Sides, col = as.factor(Sides))) + labs(x = "Player Positions", y = "Percentage of Total", 
       title = "Frequency of Board Positions", subtitle = "(according to number of dice sides)") +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
line_plot + geom_line() + geom_point() + scale_color_manual(values=c("blue1", "brown", "yellow2", "green3"))

heat_map = ggplot(data = dice_sides, aes(x = factor(Sides), y = Die)) # Create heatmap of data
heat_map + geom_tile(aes(fill = Percentage)) + labs(x = "Number of Sides", y = "Board Position", 
  title = "Heatmap of the Player Positions", subtitle = "(for 3, 4, 5, and 6-sided die)") + 
  scale_x_discrete(labels = c(3,4,5,6)) + scale_fill_gradient2(midpoint = 3) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# 3

set.seed(141)
estimate_monopolySE = function(d) { # Estimate monopoly for the standard error
  simulation = as.data.frame(table(simulate_monopoly(10000, d))) # 10,000 turns
  simulation$Freq = simulation$Freq/10000 # Make into decimal frequency
  
  return(simulation)
}

jail_se = replicate(1000, estimate_monopolySE(d = 6)[11,2]) # Save the 10th position for 1,000 iterations
sd(jail_se) # Obtain the standard error, 0.002078264

hist((jail_se-mean(jail_se))/sd(jail_se), 
     main = "Z-scores", xlab = "Standard Deviation") # Observe the standard deviations and look for outliers
hist(jail_se)

### Part 2

# 1

rent = properties$Revenue # Payment for each color

charge_rent = function(current_position, rent, property_location) {
  if (current_position %in%  property_location) { # Check if player lands on property
    bill = (-rent[which(property_location == current_position)]) # Less the corresponding bill
  } else {
    bill = 0
  }
  return(bill)
} # Charge rent for color properties

community_chest2 = function(current_position, current_deck) { # Community Chest
  if (current_position == 2 || current_position == 17 || current_position == 33) {
    current_deck_draw = current_deck[1] # Draw card
    if (current_deck_draw == 1) {
      current_position = 0 # sends player to GO
    } else if (current_deck_draw == 2) {
      current_position = 10 # sends player to Jail
    }
    
    current_deck = c(current_deck[-1], current_deck[1]) # Place card on the bottom
  }
  community_result = list(current_position, current_deck) # Return the deck, position, and money
  return(community_result)
} # Community Chest cards

chance_deck2 = function(current_position, current_deck) {
  if (current_position == 7 || current_position == 22 || current_position == 36) { # Chance
    current_deck_draw = current_deck[1] # Draw card
    if (current_deck_draw == 1) { # check if card drawn is GO or JAIL
      current_position = 0 # sends player to GO
    } else if (current_deck_draw == 2) {
      current_position = 10 # sends player to JAIL
    } else if (current_deck_draw == 3) {
      current_position = 11 # sends player to C1
    } else if (current_deck_draw == 4) {
      current_position = 24 # sends player to E3
    } else if (current_deck_draw == 5) {
      current_position = 39 # sends player to H2
    } else if (current_deck_draw == 6) {
      current_position = 5 # sends player to R1
    } else if (current_deck_draw == 7 | current_deck_draw == 8) { # Check Railroad
      index = current_position
      if (index < 5) {
        current_position = 5
      } else if (index < 15) {
        current_position = 15
      } else if (index < 25) {
        current_position = 25
      } else if (index < 35) {
        current_position = 35
      }
    } else if (current_deck_draw == 9) { # Check Utility
      index = current_position
      if (index < 12) {
        current_position = 12
      } else {
        current_position = 28
      }
    } else if (current_deck_draw == 10) { # Check if go back 3 steps
      index = current_position
      if (index >= 3) {
        index = index - 3
        current_position = index
      } else if (index == 2) {
        current_position = 39
      } else if (index == 1) {
        current_position = 38
      } else {
        current_position = 37
      }
    }
    current_deck = c(current_deck[-1], current_deck[1]) # Place card on the bottom
  }
  chance_result = list(current_position, current_deck) # Return the deck, position, and money
  return(chance_result)
} # Chance Deck cards

tax_or_go = function(current_position) {
  cash = 0 # Allocate memory for cash
  if (current_position == 4) { # Check if current position is T1 (4, -$200) or T2 (38, -$100)
    cash = -200
  } else if (current_position == 38) {
    cash = -100
  } else if (current_position == 0) { # Check if current position is GO (0, +$200)
    cash = 200
  }
  return(cash)
} # Tax or GO charges

pass_go = function(player_positions, turn) {
  if (turn > 2) {
    if ((player_positions[turn - 1] <= 39 & # Check if previous position was below 0
         player_positions[turn - 1] >= 29)
        & # Check if current position is above 0
        (player_positions[turn] >= 1 & player_positions[turn] <= 11)
        & (player_positions[turn - 1] != 36)) {
      pay = 200 # Awards 200
    } else {
      pay = 0
    }
  } else {
    pay = 0
  }
  return(pay)
} # Award 200 for passing GO

bail = function(cash1, cash2, cash3, current_position) {
  if (current_position == 10) {
    cash1 = cash2 = cash3 = 0
  }
  return(list(cash1, cash2, cash3))
} # Award no money if player ends in jail

payday = function(cash1, cash2, cash3) {
  cash_sum = cash1 + cash2 + cash3 # Sum cash per roll
  if (cash_sum != 0) {
    pay = cash_sum
  } else {
    pay = 0
  }
  return(list(pay, 0))
} # Collect cash per roll

simulate_monopoly2 = function(n, d, property, cost) {
  # Return n+1 vector of positions, along with money gained or lost each turn
  chance_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize chance deck
  chance <- sample(chance_cards) # Shuffle cards
  community_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize community chest deck
  community <- sample(community_cards) # Shuffle cards
  
  die1 = sample(1:d, n, replace = T) # Roll die 1
  die2 = sample(1:d, n, replace = T) # Roll die 2
  dice = die1 + die2 # Combine dice into vector
  dice_vec = die1 == die2 # Check for pairs
  player_positions = rep(0, n) # Allocate memory for player vector
  cash_flow = rep(0, n) # Allocate memory for cash vector
  
  current_position = 0 # Initialize current position
  for (i in 1:n) {
    current_position = dice[i] + current_position # Update position
    current_position = loop_39(current_position) # Check for loop
    
    current_position = triple(current_position = current_position, # Check for three consecutive pairs
                              turn = i, dice_vector = dice_vec)
    
    chance_list = chance_deck2(current_position = current_position, # Check for chance deck
                               current_deck = chance)
    current_position = chance_list[[1]] # Update current position
    chance = chance_list[[2]] # Update chance deck

    cc_list = community_chest2(current_position = current_position, # Check for community chest
                               current_deck = community)
    current_position = cc_list[[1]] # Update current position
    community = cc_list[[2]] # Update community chest

    cash1 = tax_or_go(current_position = current_position) # Check if Tax or GO      
    
    cash2 = pass_go(player_positions = player_positions, 
                    turn = i) # Award 200 for passing GO
    
    cash3 = charge_rent(current_position = current_position, 
                        property_location = property,rent = cost) # Charge rent for a color
    
    current_position = jail_30(current_position = current_position) # Check if player is on G2J
    
    # Award no money if player ends in jail
    cash1 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[1]]
    cash2 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[2]]
    cash3 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[3]]

    player_positions[i] = current_position # Update position vector
    
    payment = payday(cash1 = cash1, cash2 = cash2, cash3 = cash3) # Sum cash per roll
    cash_flow[i] = payment[[1]] # Update cash vector
    cash1 = cash2 = cash3 = payment[[2]] # Reset cash values to 0 after each roll
  }
  
  player_positions = c(0, player_positions) # Include initial position
  cash_flow = c(0, cash_flow) # Include initial position
  simulation = list(player_positions, cash_flow) # Output list of position and cash
  return(simulation)
} # Simulate monopoly 2

# 2

estimate_monopoly2 = function(n, d, k, property, cost) {
  #k = 1000 # Number of simulations
  property_bill = rep(0, k)
  position_list = lapply(1:k, 
    function(x) simulate_monopoly2(n, d, property, cost)) # Generate k simulations of monopoly
  
  for (i in 1:k) {
    property_bill[i] = sum(position_list[[i]][[2]]) # Return the cash vector from each simulation 
  }
  
  return(property_bill)
} # Perform estimations of the simulation

color_simulations = function(n, d, k, property, cost) {
  color_levels = levels(property$Color) # Extract different levels of color from data
  n_color = length(levels(property$Color)) # Use as variable for number of matrix columns
  color_cash = matrix(0, k, n_color) # Create matrix for the color results
  colnames(color_cash) = color_levels # Change matric column names

  for (i in 1:n_color) { # Obtain the k simulations for each color
    result = as.data.frame(estimate_monopoly2(n, d, k,
              property = property$Index[property$Color == color_levels[i]], cost = cost))
    result = as.numeric(result[,1])
    color_cash[,i] = result # Save results by color to matrix
  }
  
  return(color_cash)
} # Obtain the k simulations for each color

color_matrix = color_simulations(n = 100, d = 6, k = 1000, # Create color matrix
                                 property = properties, cost = rent)

# Create columns to mold into data frame
blue_vec = as.data.frame(color_matrix[,1]); green_vec = as.data.frame(color_matrix[,2])
lblue_vec = as.data.frame(color_matrix[,3]); orange_vec = as.data.frame(color_matrix[,4])
pink_vec = as.data.frame(color_matrix[,5]); purple_vec = as.data.frame(color_matrix[,6])
red_vec = as.data.frame(color_matrix[,7]); yellow_vec = as.data.frame(color_matrix[,8])

# Create column for color
purple_vec$Color = "Purple"; lblue_vec$Color = "LBlue"; pink_vec$Color = "Pink"; orange_vec$Color = "Orange"
red_vec$Color = "Red"; yellow_vec$Color = "Yellow"; green_vec$Color = "Green"; blue_vec$Color = "Blue"

change_names = function(color_data) {
  colnames(color_data) = c("Cash", "Color")
  return(color_data)
} # Add column names for Cash

purple = change_names(purple_vec); light_blue = change_names(lblue_vec) # Create appropriate column names
pink = change_names(pink_vec); orange = change_names(orange_vec); red = change_names(red_vec)
yellow = change_names(yellow_vec); green = change_names(green_vec); blue = change_names(blue_vec)

color_frequency = rbind(purple, light_blue, pink, orange, red, yellow, green, blue) # Combine data
density_plot = ggplot(data = color_frequency, aes(x = Cash, color = Color)) # Create density plot
density_plot + geom_density() + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(y = "Frequency of Cash Sum", x = "Cash", title = "Density Plot of Cash Sum", subtitle = "(according to colored opponents)") +
  scale_color_manual(values = c("blue", "green4", "dodgerblue1", "orange1", "deeppink1", "purple1", "red1", "yellow3")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

# 3

set.seed(141)

simulate_monopoly2b = function(n, d, property, cost) {
  # Return n+1 vector of positions, along with money gained or lost each turn
  chance_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize chance deck
  chance <- sample(chance_cards) # Shuffle cards
  community_cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) #Initialize community chest deck
  community <- sample(community_cards) # Shuffle cards
  
  die1 = sample(1:d, n, replace = T) # Roll die 1
  die2 = sample(1:d, n, replace = T) # Roll die 2
  dice = die1 + die2 # Combine dice into vector
  dice_vec = die1 == die2 # Check for pairs
  player_positions = rep(0, n) # Allocate memory for player vector
  cash_flow = rep(0, n) # Allocate memory for cash vector
  opponent_bill = rep(0, n) # Allocate memory for bill to opponent
  
  current_position = 0 # Initialize current position
  
  for (i in 1:n) {
    current_position = dice[i] + current_position # Update position
    current_position = loop_39(current_position) # Check for loop
    
    current_position = triple(current_position = current_position, # Check for three consecutive pairs
                              turn = i, dice_vector = dice_vec)
    
    chance_list = chance_deck2(current_position = current_position, # Check for chance deck
                               current_deck = chance)
    current_position = chance_list[[1]] # Update current position
    chance = chance_list[[2]] # Update chance deck
    
    cc_list = community_chest2(current_position = current_position, # Check for community chest
                               current_deck = community)
    current_position = cc_list[[1]] # Update current position
    community = cc_list[[2]] # Update community chest
    
    cash1 = tax_or_go(current_position = current_position) # Check if Tax or GO      
    
    cash2 = pass_go(player_positions = player_positions, 
                    turn = i) # Award 200 for passing GO
    
    cash3 = charge_rent(current_position = current_position, 
                        property_location = property, rent = cost) # Charge rent for a color
    opponent_bill[i] = -cash3 # Money owed to opponent (changed to positive value)
    
    current_position = jail_30(current_position = current_position) # Check if player is on G2J
    
    # Award no money if player ends in jail
    cash1 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[1]]
    cash2 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[2]]
    cash3 = bail(cash1 = cash1, cash2 = cash2, cash3 = cash3, current_position = current_position)[[3]]
    
    player_positions[i] = current_position # Update position vector
    
    payment = payday(cash1 = cash1, cash2 = cash2, cash3 = cash3) # Sum cash per roll
    cash_flow[i] = payment[[1]] # Update cash vector
    cash1 = cash2 = cash3 = payment[[2]] # Reset cash values to 0 after each roll
  }
  
  player_positions = c(0, player_positions) # Include initial position
  cash_flow = c(5000, cash_flow) # Include initial position
  opponent_bill = c(0, opponent_bill) # Include initial position
  simulation = list(player_positions, cash_flow, # Output list of position, cash, opponent bill, and construction
                    opponent_bill)
  return(simulation)
} # Simulate monopoly 2b (also pays opponent)

estimate_monopoly2b = function(n, d, property, cost) {
  k = 1
  property_bill = opponent_bill = 0
  
  position_list = lapply(1:k, function(x) simulate_monopoly2b(n, d, property, cost)) # Generate k simulations of monopoly
  
  for (i in 1:k) {
    property_bill = property_bill + sum(position_list[[k]][[2]])    
    opponent_bill = opponent_bill + sum(position_list[[k]][[3]])
  }
  
  return(list(property_bill, opponent_bill))
} # Performs one simulation

color_cash_balance = function(n, d, color_pairs, property_data, revenue) {
  primary = paste(color_pairs$color1) # Get colors for pairwise matchups
  secondary = paste(color_pairs$color2) # Second set of colors
  
  color1_PL_vector = rep(0, length(color_pairs)) # Allocate memory for color 1 vector
  color2_PL_vector = rep(0, length(color_pairs)) # Allocate memory for color 2 vector
  
  for (i in 1:length(primary)) { # Cycle through pairwise combinations and determine Profit and Loss for each
    primary_color = property_data$Index[property_data$Color == primary[i]] # Retrieve the indices of color 1
    secondary_color = property_data$Index[property_data$Color == secondary[i]] # Retrieve the indices of color 2
    
    # Inverse colors to determine Profit / Loss
    color1 = estimate_monopoly2b(n, d, property = secondary_color, cost = revenue) # Retrieve color 1's list
    color2 = estimate_monopoly2b(n, d, property = primary_color, cost = revenue) # Retrieve color 2's list
    
    color1_construction = sum(property_data$Cost[property_data$Color == primary[i]]) # Color 1 construction cost
    color2_construction = sum(property_data$Cost[property_data$Color == secondary[i]]) # Color 2 construction cost
    
    color1_PL = sum(color1[[1]]) + sum(color2[[2]]) - color1_construction # Determine P/L for color 1
    color2_PL = sum(color2[[1]]) + sum(color1[[2]]) - color2_construction # Determine P/L for color 2
    
    color1_PL_vector[i] = color1_PL # Save color 1 results to vector
    color2_PL_vector[i] = color2_PL # Save color 2 results to vector
  }
  return(list(color1_PL_vector, color2_PL_vector))
} # Simulate all pairwise games

simulation_100 = function(n) {
  simulation_matrix = matrix("", ncol=B, nrow=28)
  B = 100
  for (i in 1:B) {
    result = color_cash_balance(n,6,colors,properties,rent)
    simulation_matrix[,i] = sapply(1:28, function(x) ifelse(result[[1]][x] == result[[2]][x], NA, 
                            colors_character[x,(result[[1]][x] < result[[2]][x])+1]))
  }
  return(simulation_matrix)
} # Create 100 simulation of matchups for given color pairs

games_25 = simulation_100(25) # Simulate n = 25, 50, and 100 turns for k = 100 times
games_50 = simulation_100(50)
games_100 = simulation_100(100)

table(games_25)/(7*B)*100 # Set up a table of the results, and determine the win rate %
table(games_50)/(7*B)*100
table(games_100)/(7*B)*100
