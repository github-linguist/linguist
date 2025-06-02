make_graph() {
    if [ -f dbschema.dot ]; then
        rm -f dbschema.dot
    fi

    # NB graph_models is one of the additions from django-extensions
    python manage.py graph_models -a -X CaptchaStore,ContentType,Group,Permission,Site,Session,AbstractBaseSession,LogEntry,FileBrowser,PermissionsMixin,AbstractBaseUser,TaskResult,PeriodicTask,IntervalSchedule,PeriodicTasks,ClockedSchedule,CrontabSchedule,SolarSchedule, -x last_updated_by -g > dbschema.dot

    # Swap the direction of the arrow
    sed -i -e 's/arrowhead=none, arrowtail=dot/arrowhead=vee, arrowtail=none/g' dbschema.dot
    sed -i -e 's/arrowhead=dot, arrowtail=none/arrowhead=none, arrowtail=vee/g' dbschema.dot
    sed -i -e 's/arrowhead=dot arrowtail=dot/arrowhead=vee arrowtail=vee/g' dbschema.dot

    # https://www.graphviz.org/doc/info/attrs.html
    for layout in dot fdp sfdp osage; do  # neato  twopi  circo  patchwork
        if [ -f dbschema_$layout.pdf ]; then
            rm -f dbschema_$layout.pdf
        fi
        dot -K$layout -Tpdf dbschema.dot -x > dbschema_$layout.pdf
    done
}
