
delete from evaluation where id in (select A.id from evaluation A, evaluation B where A.version = B.version and A.test = B.test and A.checksum = B.checksum and A.id < B.id group by A.id);
