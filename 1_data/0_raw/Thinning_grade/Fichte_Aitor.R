A-grades

  if(K$versuch[i]==5 & K$parzelle[i]==1) {K$agrad[i] <- 0 }
  if(K$versuch[i]==606 & K$parzelle[i] %in% c(4)) {K$agrad[i] <- 0 }
  if(K$versuch[i]==612 & K$parzelle[i] %in% c(7)) {K$agrad[i] <- 0 } 
  if(K$versuch[i]==67 & K$parzelle[i]==1) {K$agrad[i] <- 0 }  
  if(K$versuch[i]==68 & K$parzelle[i]==1) {K$agrad[i] <- 0 } 
  if(K$versuch[i]==639 & K$parzelle[i]==1) {K$agrad[i] <- 0 }  
  if(K$versuch[i]==622 & K$parzelle[i] %in% c(3)) {K$agrad[i] <- 0 } 
  if(K$versuch[i]==613 & K$parzelle[i] %in% c(4)) {K$agrad[i] <- 0 }
  if(K$versuch[i]==603 & K$parzelle[i] %in% c(2)) {K$agrad[i] <- 0 }
  if(K$versuch[i]==602 & K$parzelle[i] %in% c(1)) {K$agrad[i] <- 0 } 
  if(K$versuch[i]==607 & K$parzelle[i] %in% c(3)) {K$agrad[i] <- 0 }
  

 B-grades
 
  if(K$versuch[i]==5 & K$parzelle[i]==2) {K$bgrad[i] <- 0 }
  if(K$versuch[i]==67 & K$parzelle[i]==2) {K$bgrad[i] <- 0 } 
  if(K$versuch[i]==68 & K$parzelle[i]==2) {K$bgrad[i] <- 0 } 
  if(K$versuch[i]==639 & K$parzelle[i]==4) {K$bgrad[i] <- 0 }  
  if(K$versuch[i]==603 & K$parzelle[i] %in% c(1)) {K$bgrad[i] <- 0 }
  if(K$versuch[i]==607 & K$parzelle[i] %in% c(8)) {K$bgrad[i] <- 0 }
  
 
 
 C-grades
 
  if(K$versuch[i]==5 & K$parzelle[i]==3) {K$cgrad[i] <- 0 }
  if(K$versuch[i]==67 & K$parzelle[i]==3) {K$cgrad[i] <- 0 } 
  if(K$versuch[i]==68 & K$parzelle[i]==3) {K$cgrad[i] <- 0 } 
  if(K$versuch[i]==602 & K$parzelle[i]==2) {K$cgrad[i] <- 0 } 
  if(K$versuch[i]==603 & K$parzelle[i]==4) {K$cgrad[i] <- 0 } 
  if(K$versuch[i]==606 & K$parzelle[i]==6) {K$cgrad[i] <- 0 } 
  if(K$versuch[i]==607 & K$parzelle[i] %in% c(5,8)) {K$cgrad[i] <- 0}
  
  if(K$versuch[i]==639 & K$parzelle[i]==4) {K$cgrad[i] <- 0 }  

 D & E-grades

# all others are D & E-grades, i. e thinning from above
 
if(K$agrad[i]!=0 & K$bgrad[i]!=0 &K$cgrad[i]!=0) {K$dgrad[i] <- 0 }

