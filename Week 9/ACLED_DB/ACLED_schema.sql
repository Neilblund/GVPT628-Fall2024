CREATE TABLE "acled" (
	"event_id_cnty"	TEXT,
	"event_date"	TEXT,
	"year"	REAL,
	"time_precision"	NUMERIC,
	"disorder_type"	TEXT,
	"event_type"	TEXT,
	"sub_event_type"	TEXT,
	"actor1"	TEXT,
	"assoc_actor_1"	TEXT,
	"inter1"	TEXT,
	"actor2"	TEXT,
	"assoc_actor_2"	TEXT,
	"inter2"	TEXT,
	"interaction"	TEXT,
	"civilian_targeting"	TEXT,
	"iso"	REAL,
	"region"	TEXT,
	"country"	TEXT,
	"admin1"	TEXT,
	"admin2"	TEXT,
	"admin3"	TEXT,
	"location"	TEXT,
	"latitude"	TEXT,
	"longitude"	TEXT,
	"geo_precision"	REAL,
	"source"	TEXT,
	"source_scale"	TEXT,
	"notes"	TEXT,
	"fatalities"	REAL,
	"tags"	TEXT,
	"timestamp"	REAL,
	PRIMARY KEY("event_id_cnty")
	)

