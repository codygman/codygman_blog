---
layout: post
title: Migrating mysql data across schemas in Django (or anything else)
description: ""
category: 
tags: []
---

I'm aware there are solutions made to avoid this, but I found myself in the situation of only having data in mysql backups. Hopefully the following will be useful to others who fall into the same situation.


###Generate mysqldump of the remote database:
```bash
    mysqldump -u cody -h corys.dbserver.com -pmypassword name_of_database
```

###Create the database on the same server as the database data is being migrated to.
```bash
    mysqadmin create dump_db
```
   
###Insert the data
```sql
    # disable foreign key checks
    set foreign_key_checks = 0;

    # insert data and foreign key data
    insert ignore into newapp_model select * from dump_db.oldapp_model;
    insert ignore into newappmodel_modelfield select * from dump_db.oldapp_modelfield;

    # enable foreign key checks again
    set foreign_key_checks = 1;
```

###Test that the data works in the Django shell
```bash
    python manage.py shell
```

```python
    In [14]: [g.genre for g in Movie.objects.filter(title="transformers")[0].movie_genre.all()]
    Out[14]: 
    [<Genre: Action>,
     <Genre: Adventure>,
     <Genre: Science Fiction>,
     <Genre: Thriller>]

```

###Last Step: Repeat.

There are undoubtedly better ways of doing this, but it didn't this method didn't take very long. More importantly, it worked :D
