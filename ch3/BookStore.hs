data BookInfo = Book Int String [String]
                deriving Show

data MagazinInfo = Magazin Int String [String]
                deriving Show

type CustomerId = Int

type ReviewBody = String

data BookReview = BookReview BookInfo CustomerId ReviewBody

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo 
    = CreditCard CardHolder CardNumber Address
    | Cash
    | Invoice CustomerId
    deriving Show
    

book1 = Book 123456789 "Algebra of Programing" ["Richard Bird", "Oege de Moor"]

