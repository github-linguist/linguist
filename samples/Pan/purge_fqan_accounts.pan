# Template installing a script to remove all accounts with 'fqan' in
# their name. Used after fixing VOConfigTask in SCDB 2.3.2 to remove
# obsolete accounts not removed by ncm-accounts.
#
# The script is added and executed only on nodes where NODE_VO_ACCOUNTS
# is true. It is intended to be run as GLITE_BASE_CONFIG_SITE (define
# this variable to the script namespace).
#
# Michel Jouvin - 13/9/09

unique template site/misc/purge_fqan_accounts;

variable LAL_PURGE_ACCOUNTS_SCRIPT = '/tmp/purge_fqan_accounts';

include 'components/filecopy/config';

'/software/components/filecopy/services' = {
    if ( is_defined(NODE_VO_ACCOUNTS) && NODE_VO_ACCOUNTS ) {
        debug('Adding purge_fqan_accounts');
        SELF[escape(LAL_PURGE_ACCOUNTS_SCRIPT)] = dict(
            'config', file_contents('site/misc/purge_fqan_accounts.sh'),
            'owner', 'root:root',
            'perms', '0755',
            'restart', LAL_PURGE_ACCOUNTS_SCRIPT,
        );
    } else {
        debug(format('VO accounts disabled (NODE_VO_ACCOUNTS=%s', NODE_VO_ACCOUNTS));
    };
    SELF;
};
