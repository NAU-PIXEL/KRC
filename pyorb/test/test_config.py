from pathlib import Path
import pyorb.config as config
import pytest

assets_dir = config.test_kernels_dir


def test_load_config():
    #test nominal case
    p_dir, k_dir = config.load_config(f"{assets_dir}/input/config_test1.toml")

    assert p_dir == Path("/path/to/porb_defaults")
    assert k_dir == Path("/path/to/kernels_cache")

    # test missing file
    with pytest.raises(FileNotFoundError):
        p_dir, k_dir = config.load_config(f"{assets_dir}/input/config_test_NONEXISTENT_DUMMY.toml")

    # Test missing values
    with pytest.raises(KeyError):
        p_dir, k_dir = config.load_config(f"{assets_dir}/input/config_test2.toml")
    with pytest.raises(ValueError):
        p_dir, k_dir = config.load_config(f"{assets_dir}/input/config_test3.toml")

def test_install_config():
    # out_config_file = f"{assets_dir}/output/config_test1.toml"
    # TODO: write this test. the function takes command line input, so I don't know how to do that.

    assert True


