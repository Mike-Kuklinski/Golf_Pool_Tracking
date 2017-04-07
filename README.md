#Golf Major - Pool Tracker

**Author:** Mike Kuklinski  
**Date:** 5/20/16
**Update:** 6/20/16  
**Language:** R  
**Computer:** Windows 10, 64-bit  
**Description:**  

Enclosed is the code I used to deploy a web application (shinyapp) for the purposes of tracking a golf pool bracket for the four PGA Majors (most recently the Masters). Application allows for pool entry participants to get up-to-date golfer and pool entry standings. I've also included features for checking how many
other entries have identical player combinations as well as generating a 'best case' scenario in the pool for a given entry. This provides a fun way for participants to track their
performance and know which golfers to root for to maximize their standings in the pool.
  
Updates:    
- Tournament Id and pool entry information now stored on Google Drive Account, from which the website will pull its information. This is in lieu of having a hard copy of the same information published with the website.  By storing all information on the Google Drive account, the website won't have to be re-published with new tournament information in the future. Instead, it can just be altered on the google drive.   
- Added maintenance tab for authorized users. User can update the tournament information (id, purse amount, year, name), pool entries, payout percentages directly on the website. Changes made will automatically update the google drive account.  
- Added Google analytics to track users and actions taken on the website.  
- Miscellaneous Bug fixes.   

Shinyapp Website:
[Golf Pool Tracker](https://mike-kuklinski.shinyapps.io/boomer_pool/)
