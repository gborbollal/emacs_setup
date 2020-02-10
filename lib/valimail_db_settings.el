(setq defend_dbs
      '('("valkyrie_local"
	  (sql-product 'postgres)
	  (sql-user "postgres")
	  (sql-password "")
	  (sql-server "localhost")
	  (sql-database "valkyrie_dev")
	  (sql-port 5433))
	'("valkyrie_staging"
	  (sql-product 'postgres)
	  (sql-user "defenduser")
	  (sql-server "localhost")
	  (sql-database "defendpii")
	  (sql-port 5438))))
