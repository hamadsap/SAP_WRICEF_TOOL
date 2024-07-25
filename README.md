# SAP_WRICEF_TOOL
SAP WRICEF Tool helps to get SAP objects against Transaction Code and to find Database Table and CDS View

* [Download Zip File](https://github.com/hamadsap/SAP_WRICEF_TOOL/archive/refs/heads/main.zip)
* [Install ABAPGIT Program and Execute (SE38)](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap)
* Click New Offline

![image](https://github.com/user-attachments/assets/b06a71d7-7eb4-45b0-bd08-65760d5135c4)
* Create Package
* Create Offline Repository
* Import Zip File
* Pull Zip
* Check Object, Continue, and Activate

![image](https://github.com/user-attachments/assets/849ec6e1-7c70-4330-9bbf-b43f27743209)

* GoTo SICF Transaction
* Activate Service

![image](https://github.com/user-attachments/assets/07e7b3e0-79bc-4c24-9885-e9e5963e3215)

* Fill Logon Data and Save
  * Client
  * Your SAP User
  * Password

![image](https://github.com/user-attachments/assets/98740d5d-bd20-4473-87e6-9bf3e460f07b)

* Test Service and Note Server and port

![image](https://github.com/user-attachments/assets/f437db68-f80b-44d5-ad6d-f93b3aa491e0)

* Test API Get Request [Change Server and Port]
[Example API](https://host.sap.pk:44300/zgettab/GetTables?sap-client=100&TCODE=PA30&OBJECT=T)
