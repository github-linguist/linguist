static const syscalldef syscalldefs[] = {
        [SYSCALL_OR_NUM(0, SYS_restart_syscall)]        = MAKE_UINT16(0, 1),
        [SYSCALL_OR_NUM(1, SYS_exit)]         = MAKE_UINT16(1, 17),
        [SYSCALL_OR_NUM(2, SYS_fork)]         = MAKE_UINT16(0, 22),
};
