

SELECT "quote_entry"."id", "quote_entry"."body", "quote_entry"."manual_attribution", "quote_entry"."attribution_tag"
    FROM "quote_entry" LEFT OUTER JOIN "read_entry" ON "quote_entry"."attribution_tag" = "read_entry"."id"
    WHERE ((1 AND 1) OR (1 AND 1)) AND ((("quote_entry"."body" LIKE "%nature%") AND 1) 
      AND (("quote_entry"."id" >= '2019-03-23T00:00:00') AND ("quote_entry"."id" <= '2019-09-20T00:00:00')))

