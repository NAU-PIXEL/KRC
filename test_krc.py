import pytest
import subprocess
import tables
import os
import numpy as np
# We're gonna have to use Davinci for most of this. 
# Basic procedure for most of these tests will look like this:
# 1. Do an initial run myself to get the canonical output for each case, saving to an hdf5 file
# 2. Run davinci with the KRC command that generated the original output
# 3. Write that output from Davinci to an hdf5 file
# 4. Back in Python, load the 2 files and make sure they agree. 

def run_davinci(cmd, output_file, setup = ""):
  davinci_string = f"""
  {setup}
  a = {cmd}
  write(a, "{output_file}", hdf, force=1)
  """
  subprocess.run(["davinci"], input=davinci_string, text=True)

# then use that davinci wrapper in a Python function that will load the HDF5 and return the object

def load_davinci_output(output_file):
  f = tables.open_file(output_file, "r")
  return f


def generate_ground_truth():
  # alright let's write some code that will generate our initial data to verify against

  os.chdir("hdf_test/ground_truth")
  run_davinci('porb("Mars", force=1)', "mars_porb.hdf")
  run_davinci('porb("Bennu", force=1)', "bennu_porb.hdf")
  run_davinci('porb("Phobos", force=1)', "phobos_porb.hdf")
  run_davinci('porb("Europa", force=1)', "europa_porb.hdf")
  run_davinci('porb("2688_Halley", force=1)', "halley_porb.hdf")
  run_davinci('porb("Ceres", force=1)', "ceres_porb.hdf")
  jupiter_trojan_setup = """
  poledec=0
  tmp=generic_porb(e=0, a=1.,i=1.30439695, node=100.47390909, peri=293.923, m=79.668,rot_per=200., polera=273.85,poledec=poledec, merid=7.7, period=4332.589, name="IdealJupiterTrojan")
  """
  run_davinci('krc(tmp, force=1)', "jupiter_trojan_porb.hdf", jupiter_trojan_setup)

  exoplanet_setup = """
  name       = "Exoplanet_0001"
  epoch      = 2451545.00
  VisMag     = 0.
  DisEarth   = 0.
  e          = 0.5
  a          = 1.2
  rot_per    = 365.256
  period     = 23.9345
  Obliq      = 45.
  tmp        = exo_porb(name=name,epoch=epoch,VisMag=VisMag,DisEarth=DisEarth,e=e,a=a,rot_per=rot_per,period=period,Obliq=Obliq)
  tmp_porb = porb(tmp, force=1)
  """
  run_davinci('krc(lat=0.,body=tmp_porb,PTOTAL=0.,DUSTA=0.,GRAV=0.,TAURAT=0.,ARC2_G0=0.,TFROST=0.)', "exoplanet_krc.hdf", exoplanet_setup)

  run_davinci("krc(lat=12.,GD=\"1994-Feb-05\")", "y_krc.hdf")
  run_davinci("krc(lat=12.,JD=2450075.5)", "x_krc.hdf")
  run_davinci("krc(lat=12.,DJUL=-1000.,LKEY=\"T\")", "w_krc.hdf")
  run_davinci("krc(lat=12.,body=tmp)", "z_krc.hdf")
  run_davinci("krc(KEEP=\"T\",lat=12.)", "a_krc.hdf")
  run_davinci("krc(lat=12.,hour=2.45,T=172.3,ls=23.)", "b_krc.hdf")
  run_davinci("krc(lat=12.,hour=2.45,T=T,ls=23.)", "b2_krc.hdf", setup="T = create(12,12,1,start=146,step=1,format=float)")
  run_davinci("krc(lat=12.,hour=2.45,T=82.3,ls=23.,body=\"Europa\")", "b3_krc.hdf")
  run_davinci("krc(lat=12.,body=\"Ceres\",bodyforce=1)", "c_krc.hdf")
  run_davinci("krc(lat=12.,body=\"Phobos\",bodyforce=1)", "d_krc.hdf")
  run_davinci("krc(lat=12.,body=\"Europa\",bodyforce=1)", "e_krc.hdf")
  run_davinci("krc(lat=12.,body=\"Phobos\",PFlux=\"T\")", "f_krc.hdf")
  run_davinci("krc(lat=25.,ls=90.,body=\"2688_Halley\",N1=30)", "g_krc.hdf")
  run_davinci("krc(lat=0.,INERTIA=50.,body=\"Phobos\",LKofT=\"T\",PFlux=\"F\")", "out_noflux_krc.hdf")
  run_davinci("krc(lat=0.,INERTIA=50.,body=\"Phobos\",LKofT=\"T\",PFlux=\"T\",Lon_Hr=12.)", "out_flux_krc.hdf")

  # let's call that enough base cases for now, while I don't know what specifically we're testing
  # the original test cases Sylvain gave me was 1000 lines of Davinci and it's not clear to me what the cases are comparing to each other.
  # some of them have a number associated with them, but I don't see what the number references.

def exoplanet_krc():
  exoplanet_setup = """
  name       = "Exoplanet_0001"
  epoch      = 2451545.00
  VisMag     = 0.
  DisEarth   = 0.
  e          = 0.5
  a          = 1.2
  rot_per    = 365.256
  period     = 23.9345
  Obliq      = 45.
  tmp        = exo_porb(name=name,epoch=epoch,VisMag=VisMag,DisEarth=DisEarth,e=e,a=a,rot_per=rot_per,period=period,Obliq=Obliq)
  tmp_porb = porb(tmp, force=1)
  """
  run_davinci('krc(lat=0.,body=tmp_porb,PTOTAL=0.,DUSTA=0.,GRAV=0.,TAURAT=0.,ARC2_G0=0.,TFROST=0.)', "exoplanet_krc.hdf", exoplanet_setup)
  return load_davinci_output("exoplanet_krc.hdf")

def gd_krc():
  run_davinci("krc(lat=12.,GD=\"1994-Feb-05\")", "y_krc.hdf")
  return load_davinci_output("y_krc.hdf")

def jd_krc():
  run_davinci("krc(lat=12.,JD=2450075.5)", "x_krc.hdf")
  return load_davinci_output("x_krc.hdf")

def djul_krc():
  run_davinci("krc(lat=12.,DJUL=-1000.,LKEY=\"T\")", "w_krc.hdf")
  return load_davinci_output("w_krc.hdf")

def jupiter_trojan_krc():
  jupiter_trojan_setup = """
  poledec=0
  tmp=generic_porb(e=0, a=1.,i=1.30439695, node=100.47390909, peri=293.923, m=79.668,rot_per=200., polera=273.85,poledec=poledec, merid=7.7, period=4332.589, name="IdealJupiterTrojan")
  body = porb(tmp)
  """
  run_davinci("krc(lat=12.,body=body)", "z_krc.hdf", jupiter_trojan_setup)
  return load_davinci_output("z_krc.hdf")

# let's test all the parameters that come out of the HDF5 file
# tsurf, tbol, tatm, down_ir, down_vis, time, ls, deltaJD, layer.mass_burden, layer.density, 
# layer.m.thickness, layer.m.center_depth, layer.m.depth,
# layer.tmin, layer.tmax
# anc.ref_pressure, anc.taud, anc.total_frost, anc.avg_heat_flow, anc.frost_alb, anc.frost, anc.tatm_predict, anc.delta_t_rms,
# anc.converge_days

# that's a lot of setup for just one planet's test. Instead we actually write a base class that has 
# those tests, then set the data up in the child classes

reference_dir = "hdf_test/arraydiff_reference"

class BaseTestPlanet():
  planet: tables.File
  
  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_tsurf(cls):
    return cls.planet.root.tsurf.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_tbol(cls):
    return cls.planet.root.tbol.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_tatm(cls):
    return cls.planet.root.tatm.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_down_ir(cls):
    return cls.planet.root.down_ir.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_down_vis(cls):
    return cls.planet.root.down_vis.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_time(cls):
    return cls.planet.root.time.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_ls(cls):
    return cls.planet.root.ls.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_deltaJD(cls):
    return cls.planet.root.deltaJD.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_mass_burden(cls):
    return cls.planet.root.layer.mass_burden.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_density(cls):
    return cls.planet.root.layer.Density.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_m_thickness(cls):
    return cls.planet.root.layer.m.thickness.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_m_center_depth(cls):
    return cls.planet.root.layer.m.center_depth.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_m_depth(cls):
    return cls.planet.root.layer.m.depth.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_tmin(cls):
    return cls.planet.root.layer.tmin.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_layer_tmax(cls):
    return cls.planet.root.layer.tmax.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_ref_pressure(cls):
    return cls.planet.root.anc.ref_pressure.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_taud(cls):
    return cls.planet.root.anc.taud.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_total_frost(cls):
    return cls.planet.root.anc.total_frost.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_avg_heat_flow(cls):
    return cls.planet.root.anc.avg_heat_flow.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_frost_alb(cls):
    return cls.planet.root.anc.frost_alb.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_frost(cls):
    return cls.planet.root.anc.frost.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_tatm_predict(cls):
    return cls.planet.root.anc.tatm_predict.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_delta_t_rms(cls):
    return cls.planet.root.anc.delta_t_rms.read()

  @pytest.mark.array_compare(derive_classes = True, reference_dir = reference_dir)
  def test_anc_converge_days(cls):
    return cls.planet.root.anc.converge_days.read()


class TestExoplanet(BaseTestPlanet):
  planet = exoplanet_krc()

class TestGD(BaseTestPlanet):
  planet = gd_krc()

class TestJD(BaseTestPlanet):
  planet = jd_krc()

class TestDJUL(BaseTestPlanet):
  planet = djul_krc()

class TestJupiterTrojan(BaseTestPlanet):
  planet = jupiter_trojan_krc()


if __name__ == "__main__":
  # generate_ground_truth()
  pass