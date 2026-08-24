import os
import pytest
import requests

from pathlib import Path


@pytest.fixture(scope="session")
def download_de442_spk(request):
    dest = request.config.rootpath / "test/kernels/input/test1/spk/de442.bsp"
    url = "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de442.bsp"
    if not dest.is_file():
        r = requests.get(url)
        os.makedirs(dest.parent, exist_ok=True)
        with open(dest, "wb") as f:
            f.write(r.content)
    return dest
