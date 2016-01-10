


load_all('E:/Codes/hydrometeo')

tropSST <- querydb(fileName = 'ersst/ersst.db',
             statement = 'select * from datatable
             where lat between -15 and 15 and lon between 90 and 290 
             and year between 1961 and 2000')

save(tropSST, file = './data/tropSST.rda')
            
