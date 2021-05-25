import xml.etree.ElementTree as ET
from faker import Faker

faker = Faker()
tree = ET.parse("public/summary.xml")
root = tree.getroot()
i = 0

for study in root.iter("study"):
    study.find("patientname").text = faker.name()
    study.find("NHI").text = "ABC{0:03d}".format(i)
    i += 1

tree.write("public/anonymised_summary.xml")
