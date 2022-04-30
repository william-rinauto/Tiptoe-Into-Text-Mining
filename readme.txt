fintech review.R uses functions that I wrote to scrape trustpilot.com for several companies

sofi = 'https://www.trustpilot.com/review/sofi.com',
ondeck = 'https://www.trustpilot.com/review/ondeck.com',
lendingclub = 'https://www.trustpilot.com/review/lendingclub.com',
lendingree = 'https://www.trustpilot.com/review/www.lendingtree.com',
kabbage = 'https://www.trustpilot.com/review/www.kabbage.com',
prosper = 'https://www.trustpilot.com/review/www.prosper.com',
macrus = 'https://www.trustpilot.com/review/www.marcus.co.uk',
chase = 'https://www.trustpilot.com/review/chase.com',
credibility_capital = 'https://www.trustpilot.com/review/credibilitycapital.com',
funding_circle = 'https://www.trustpilot.com/review/fundingcircle.com',
bhg = 'https://www.trustpilot.com/review/bankershealthcaregroup.com'

The reviews get stored to a local sqlite database.

Then in messing around text minig.R, I apply some sentiment analysis techniques that I learned in tidytext mining.

Most of the interesting information is contained in plots.R