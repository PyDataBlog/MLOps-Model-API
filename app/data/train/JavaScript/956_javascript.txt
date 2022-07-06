import { get_definition } from './../base';

export const push_link = (
    oid, linkurl, linkname, onmenu='true', instance_name,
    when, additional_args, description
) => get_definition({
        oid,
        linkurl,
        linkname,
        onmenu,
        instance_name
    },
    {
        label: 'VersionOne - Push Link',
        method: 'push_link',
        module: 'main',
        name: 'v1plugin'
    },
    when, additional_args, description
);