
insert into variant(submission, name) select id, 'out' from submission;

update out
set variant=subquery.variant_id
from (select O.id as out_id, V.id as variant_id
      from out as O, variant as V, submission as S
      where V.submission = S.id and O.submission = S.id) AS subquery
where out.id = subquery.out_id;
