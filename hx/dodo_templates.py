from pathlib import *
from hx.dodo_helpers import ExpandedTemplate

ROOT = Path(__file__).parent.parent.parent.parent

def spa_nginx_config(
    backend_host,
    backend_port,
    json_logging=True,
    server_name=None
    ):
    return ExpandedTemplate(
        ROOT/'helix/nginx-templates/nginx-spa.conf.tpl',
        substitutions = {
            'backend_host' : backend_host,
            'backend_port' : backend_port,
            'json_logging' : json_logging,
            'server_name'  : server_name,
        }
    )
