-- If an SQL error occurs, just ignore it.
WHENEVER SQLERROR CONTINUE NONE
-- Try to drop the user FCS and all of the objects it owns.
DROP USER FCS_BIOSUM CASCADE;
EXIT