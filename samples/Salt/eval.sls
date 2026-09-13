ceph:
  pkg.installed:
    - refresh: True
  service:
    - dead
    - enable: False
    - require:
      - file: /etc/eval.conf
   {% if grains['os'] == 'Ubuntu'%}
      - file: /etc/apt/sources.list.d/ceph.list
   {% endif %}

ceph-mds:
  pkg.installed:
    - require:
      - pkg: ceph

include:
  - ceph.extras

{% if grains['os'] == 'Ubuntu'%}
/etc/apt/sources.list.d/ceph.list:
  file.managed:
    - source: salt://ceph/apt.list
    - template: jinja
    - require: 
      - cmd: repo-key

repo-key:
  cmd.run:
    - name: 'wget -q -O - https://raw.github.com/release.asc | sudo apt-key add -'
    - unless: 'apt-key list | grep -q -i ceph' 
{% endif %}

/etc/ceph/ceph.conf:
  file.managed:
    - source: salt://ceph/eval.conf
    - template: jinja
    - makedirs: true

/var/lib/ceph:
  file.directory:
    - names:
     {% for dir in 'mon.a','osd.0','osd.1','mds.a' %}
      - /var/lib/ceph/{{ dir.split('.')[0] }}/ceph-{{ dir.split('.')[1] }}
     {% endfor %}
    - require:
      - pkg: ceph
