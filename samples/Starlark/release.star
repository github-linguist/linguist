load(
    'scripts/drone/steps/lib.star',
    'disable_tests',
    'clone_enterprise_step',
    'download_grabpl_step',
    'gen_version_step',
    'yarn_install_step',
    'wire_install_step',
    'init_enterprise_step',
    'lint_drone_step',
    'build_image',
    'identify_runner_step',
    'publish_image',
    'lint_backend_step',
    'lint_frontend_step',
    'codespell_step',
    'shellcheck_step',
    'test_backend_step',
    'test_backend_integration_step',
    'test_frontend_step',
    'build_backend_step',
    'build_frontend_step',
    'build_frontend_package_step',
    'build_plugins_step',
    'package_step',
    'grafana_server_step',
    'e2e_tests_step',
    'e2e_tests_artifacts',
    'build_storybook_step',
    'copy_packages_for_docker_step',
    'build_docker_images_step',
    'postgres_integration_tests_step',
    'mysql_integration_tests_step',
    'redis_integration_tests_step',
    'memcached_integration_tests_step',
    'get_windows_steps',
    'benchmark_ldap_step',
    'store_storybook_step',
    'upload_packages_step',
    'store_packages_step',
    'upload_cdn_step',
    'validate_scuemata_step',
    'ensure_cuetsified_step',
    'publish_images_step',
    'trigger_oss'
)

load(
    'scripts/drone/services/services.star',
    'integration_test_services',
    'integration_test_services_volumes',
    'ldap_service',
)

load(
    'scripts/drone/utils/utils.star',
    'pipeline',
    'notify_pipeline',
    'failure_template',
    'drone_change_template',
)
load('scripts/drone/vault.star', 'from_secret', 'github_token', 'pull_secret', 'drone_token', 'prerelease_bucket')

def store_npm_packages_step():
    return {
        'name': 'store-npm-packages',
        'image': publish_image,
        'depends_on': [
            'build-frontend-packages',
        ],
        'environment': {
            'GCP_KEY': from_secret('gcp_key'),
            'PRERELEASE_BUCKET': from_secret(prerelease_bucket)
        },
        'commands': [
            './bin/grabpl artifacts npm store --tag ${DRONE_TAG}'
        ],
    }

def retrieve_npm_packages_step():
    return {
        'name': 'retrieve-npm-packages',
        'image': publish_image,
        'depends_on': [
            'yarn-install',
        ],
        'environment': {
            'GCP_KEY': from_secret('gcp_key'),
            'PRERELEASE_BUCKET': from_secret(prerelease_bucket)
        },
        'commands': [
            './bin/grabpl artifacts npm retrieve --tag v${TAG}'
        ],
    }

def release_npm_packages_step():
    return {
        'name': 'release-npm-packages',
        'image': build_image,
        'depends_on': [
            'retrieve-npm-packages',
        ],
        'environment': {
            'NPM_TOKEN': from_secret('npm_token'),
        },
        'commands': [
            './bin/grabpl artifacts npm release --tag v${TAG}'
        ],
    }

def fetch_images_step(edition):
    return {
        'name': 'fetch-images-{}'.format(edition),
        'image': 'google/cloud-sdk',
        'environment': {
            'GCP_KEY': from_secret('gcp_key'),
            'DOCKER_USER': from_secret('docker_username'),
            'DOCKER_PASSWORD': from_secret('docker_password'),
        },
        'commands': ['./bin/grabpl artifacts docker fetch --version-tag ${{TAG}} --edition {} --base alpine --base ubuntu --arch amd64 --arch arm64 --arch armv7'.format(edition)],
        'depends_on': ['grabpl'],
        'volumes': [{
            'name': 'docker',
            'path': '/var/run/docker.sock'
        }],
    }

def publish_image_steps(version, mode, docker_repo, additional_docker_repo=""):
    steps = [
        download_grabpl_step(),
        fetch_images_step(version),
        publish_images_step(version, 'release', mode, docker_repo),
    ]
    if additional_docker_repo != "":
        steps.extend([publish_images_step(version, 'release', mode, additional_docker_repo)])

    return steps

def publish_image_pipelines(mode):
    trigger = {
        'event': ['promote'],
        'target': [mode],
    }

    return [pipeline(
        name='publish-docker-oss-{}'.format(mode), trigger=trigger, steps=publish_image_steps(version='oss',  mode=mode, docker_repo='grafana', additional_docker_repo='grafana-oss'), edition=""
    ), pipeline(
        name='publish-docker-enterprise-{}'.format(mode), trigger=trigger, steps=publish_image_steps(version='enterprise',  mode=mode, docker_repo='grafana-enterprise'), edition=""
    ),]

def get_steps(edition, ver_mode):
    package_steps = []
    publish_steps = []
    should_publish = ver_mode == 'release'
    should_upload = should_publish or ver_mode in ('release-branch',)
    include_enterprise2 = edition == 'enterprise'
    edition2 = 'enterprise2'
    init_steps = [
        identify_runner_step(),
        download_grabpl_step(),
        gen_version_step(ver_mode),
        wire_install_step(),
        yarn_install_step(),
    ]

    test_steps = []
    if edition != 'enterprise':
        test_steps.extend([shellcheck_step()])

    test_steps.extend([
        codespell_step(),
        lint_backend_step(edition=edition),
        lint_frontend_step(),
        test_backend_step(edition=edition),
        test_backend_integration_step(edition=edition),
        test_frontend_step(),
    ])

    build_steps = [
        build_backend_step(edition=edition, ver_mode=ver_mode),
        build_frontend_step(edition=edition, ver_mode=ver_mode),
        build_frontend_package_step(edition=edition, ver_mode=ver_mode),
        build_plugins_step(edition=edition, sign=True),
        validate_scuemata_step(),
        ensure_cuetsified_step(),
    ]

    integration_test_steps = [
        postgres_integration_tests_step(edition=edition, ver_mode=ver_mode),
        mysql_integration_tests_step(edition=edition, ver_mode=ver_mode),
    ]


    if include_enterprise2:
        test_steps.extend([
            lint_backend_step(edition=edition2),
            test_backend_step(edition=edition2),
            test_backend_integration_step(edition=edition2),
        ])
        build_steps.extend([
            build_backend_step(edition=edition2, ver_mode=ver_mode, variants=['linux-amd64']),
        ])

    # Insert remaining steps
    build_steps.extend([
        package_step(edition=edition, ver_mode=ver_mode, include_enterprise2=include_enterprise2),
        copy_packages_for_docker_step(),
        build_docker_images_step(edition=edition, ver_mode=ver_mode, publish=True),
        build_docker_images_step(edition=edition, ver_mode=ver_mode, ubuntu=True, publish=True),
        grafana_server_step(edition=edition),
    ])

    if not disable_tests:
        build_steps.extend([
            e2e_tests_step('dashboards-suite', edition=edition, tries=3),
            e2e_tests_step('smoke-tests-suite', edition=edition, tries=3),
            e2e_tests_step('panels-suite', edition=edition, tries=3),
            e2e_tests_step('various-suite', edition=edition, tries=3),
            e2e_tests_artifacts(edition=edition),
        ])

    build_storybook = build_storybook_step(edition=edition, ver_mode=ver_mode)
    if build_storybook:
        build_steps.append(build_storybook)

    if include_enterprise2:
      integration_test_steps.extend([redis_integration_tests_step(), memcached_integration_tests_step()])

    if should_upload:
        publish_steps.append(upload_cdn_step(edition=edition, ver_mode=ver_mode, trigger=trigger_oss))
        publish_steps.append(upload_packages_step(edition=edition, ver_mode=ver_mode, trigger=trigger_oss))
    if should_publish:
        publish_step = store_storybook_step(edition=edition, ver_mode=ver_mode)
        store_npm_step = store_npm_packages_step()
        if publish_step:
            publish_steps.append(publish_step)
        if store_npm_step:
            publish_steps.append(store_npm_step)
    windows_package_steps = get_windows_steps(edition=edition, ver_mode=ver_mode)

    if include_enterprise2:
        publish_steps.extend([
            package_step(edition=edition2, ver_mode=ver_mode, include_enterprise2=include_enterprise2, variants=['linux-amd64']),
            upload_cdn_step(edition=edition2, ver_mode=ver_mode),
        ])
        if should_upload:
            step = upload_packages_step(edition=edition2, ver_mode=ver_mode)
            if step:
                publish_steps.append(step)

    return init_steps, test_steps, build_steps, integration_test_steps, package_steps, windows_package_steps, publish_steps

def get_oss_pipelines(trigger, ver_mode):
    edition = 'oss'
    services = integration_test_services(edition=edition)
    volumes = integration_test_services_volumes()
    init_steps, test_steps, build_steps, integration_test_steps, package_steps, windows_package_steps, publish_steps = get_steps(edition=edition, ver_mode=ver_mode)
    windows_pipeline = pipeline(
        name='oss-windows-{}'.format(ver_mode), edition=edition, trigger=trigger,
        steps=[identify_runner_step('windows')] + windows_package_steps,
        platform='windows', depends_on=[
            'oss-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode),
        ],
    )
    pipelines = [
        pipeline(
            name='oss-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode), edition=edition, trigger=trigger, services=[],
            steps=init_steps + build_steps + package_steps + publish_steps,
            volumes=volumes,
        ),
    ]
    if not disable_tests:
        pipelines.extend([
            pipeline(
                name='oss-test-{}'.format(ver_mode), edition=edition, trigger=trigger, services=[],
                steps=init_steps + test_steps,
                volumes=[],
            ),
            pipeline(
                name='oss-integration-tests-{}'.format(ver_mode), edition=edition, trigger=trigger, services=services,
                steps=[download_grabpl_step(), identify_runner_step(),] + integration_test_steps,
                volumes=volumes,
            )
        ])
        deps = {
            'depends_on': [
                'oss-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode),
                'oss-test-{}'.format(ver_mode),
                'oss-integration-tests-{}'.format(ver_mode)
            ]
        }
        windows_pipeline.update(deps)

    pipelines.extend([windows_pipeline])
    return pipelines

def get_enterprise_pipelines(trigger, ver_mode):
    edition = 'enterprise'
    services = integration_test_services(edition=edition)
    volumes = integration_test_services_volumes()
    deps_on_clone_enterprise_step = {
        'depends_on': [
            'init-enterprise',
        ]
    }
    _, test_steps, build_steps, integration_test_steps, package_steps, windows_package_steps, publish_steps = get_steps(edition=edition, ver_mode=ver_mode)
    init_steps = [
        download_grabpl_step(),
        identify_runner_step(),
        clone_enterprise_step(ver_mode),
        init_enterprise_step(ver_mode)
    ]
    for step in [wire_install_step(), yarn_install_step(), gen_version_step(ver_mode)]:
        step.update(deps_on_clone_enterprise_step)
        init_steps.extend([step])

    for step in integration_test_steps:
        step.update(deps_on_clone_enterprise_step)

    windows_pipeline = pipeline(
        name='enterprise-windows-{}'.format(ver_mode), edition=edition, trigger=trigger,
        steps=[identify_runner_step('windows')] + windows_package_steps,
        platform='windows', depends_on=[
            'enterprise-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode),
        ],
    )
    pipelines = [
        pipeline(
            name='enterprise-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode), edition=edition, trigger=trigger, services=[],
            steps=init_steps + build_steps + package_steps + publish_steps,
            volumes=volumes,
        ),
    ]
    if not disable_tests:
        pipelines.extend([
            pipeline(
                name='enterprise-test-{}'.format(ver_mode), edition=edition, trigger=trigger, services=[],
                steps=init_steps + test_steps,
                volumes=[],
            ),
            pipeline(
                name='enterprise-integration-tests-{}'.format(ver_mode), edition=edition, trigger=trigger, services=services,
                steps=[download_grabpl_step(), identify_runner_step(), clone_enterprise_step(ver_mode), init_enterprise_step(ver_mode),] + integration_test_steps,
                volumes=volumes,
            ),
        ])
        deps = {
            'depends_on': [
                'enterprise-build{}-publish-{}'.format(get_e2e_suffix(), ver_mode),
                'enterprise-test-{}'.format(ver_mode),
                'enterprise-integration-tests-{}'.format(ver_mode)
            ]
        }
        windows_pipeline.update(deps)

    pipelines.extend([windows_pipeline])

    return pipelines

def publish_artifacts_step(mode):
    security = ''
    if mode == 'security':
        security = '--security '
    return {
        'name': 'publish-artifacts',
        'image': publish_image,
        'environment': {
            'GCP_KEY': from_secret('gcp_key'),
            'PRERELEASE_BUCKET': from_secret('prerelease_bucket'),
        },
        'commands': ['./bin/grabpl artifacts publish {}--tag ${{TAG}} --src-bucket $${{PRERELEASE_BUCKET}}'.format(security)],
        'depends_on': ['grabpl'],
    }

def publish_packages_step(edition):
    return {
        'name': 'publish-packages-{}'.format(edition),
        'image': publish_image,
        'environment': {
            'GCP_KEY': from_secret('gcp_key'),
        },
        'commands': ['./bin/grabpl store-packages {}'.format(edition)],
        'depends_on': ['grabpl'],
    }

def publish_artifacts_pipelines(mode):
    trigger = {
        'event': ['promote'],
        'target': [mode],
    }
    steps = [
        download_grabpl_step(),
        publish_artifacts_step(mode),
    ]

    return [pipeline(
        name='publish-artifacts-{}'.format(mode), trigger=trigger, steps=steps, edition="all"
    )]

def publish_packages_pipeline():
    trigger = {
        'event': ['promote'],
        'target': ['public'],
    }
    oss_steps = [
        download_grabpl_step(),
        store_packages_step(edition='oss', ver_mode='release'),
    ]

    enterprise_steps = [
        download_grabpl_step(),
        store_packages_step(edition='enterprise', ver_mode='release'),
    ]
    deps = [
        'publish-artifacts-public',
        'publish-docker-oss-public',
        'publish-docker-enterprise-public'
    ]

    return [pipeline(
        name='publish-packages-oss', trigger=trigger, steps=oss_steps, edition="all", depends_on=deps
    ), pipeline(
        name='publish-packages-enterprise', trigger=trigger, steps=enterprise_steps, edition="all", depends_on=deps
    )]

def publish_npm_pipelines(mode):
    trigger = {
        'event': ['promote'],
        'target': [mode],
    }
    steps = [
        download_grabpl_step(),
        yarn_install_step(),
        retrieve_npm_packages_step(),
        release_npm_packages_step()
    ]

    return [pipeline(
        name='publish-npm-packages-{}'.format(mode), trigger=trigger, steps = steps, edition="all"
    )]

def release_pipelines(ver_mode='release', trigger=None):
    # 'enterprise' edition services contain both OSS and enterprise services
    if not trigger:
        trigger = {
            'event': {
                'exclude': [
                    'promote'
                ]
            },
            'ref': ['refs/tags/v*',],
            'repo': {
                'exclude': ['grafana/grafana'],
            },
        }

    # The release pipelines include also enterprise ones, so both editions are built for a release.
    # We could also solve this by triggering a downstream build for the enterprise repo, but by including enterprise
    # in OSS release builds, we simplify the UX for the release engineer.
    oss_pipelines = get_oss_pipelines(ver_mode=ver_mode, trigger=trigger)
    enterprise_pipelines = get_enterprise_pipelines(ver_mode=ver_mode, trigger=trigger)

    pipelines = oss_pipelines + enterprise_pipelines

    return pipelines

def get_e2e_suffix():
    if not disable_tests:
        return '-e2e'
    return ''
