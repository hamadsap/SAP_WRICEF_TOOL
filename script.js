document.addEventListener("DOMContentLoaded", function () {
  const tcodeInput = document.getElementById("tcode");
  const programInput = document.getElementById("program");
  const tableBody = document.querySelector("#tableResults tbody");
  const loadingDiv = document.getElementById("loading");
  const table = document.getElementById("tableResults");
  const wricef = document.getElementById("wricef");
  const submitButton = document.getElementById("submit");

  const OBJC = document.getElementById("OBJC");
  const TABL = document.getElementById("TABL");
  const CDS = document.getElementById("CDS");

  const tableCdsFields = document.getElementById("tableCdsFields");
  const findObjectSel = document.getElementById("findObjectSel");

  const Elementfield1 = document.getElementById("field1");
  const Elementfield2 = document.getElementById("field2");
  const Elementfield3 = document.getElementById("field3");
  const Elementfield4 = document.getElementById("field4");
  const radioOptions = document.querySelectorAll(".radio-option");

  const errorMessageDiv = document.getElementById("error-message");

// Get references to the icon and tooltip
const infoIcon = document.getElementById("info-icon");
const tooltip = document.getElementById("tooltip");


  // Function to show the tooltip
  function showTooltip() {
    tooltip.style.display = "block";
    setTimeout(function () {
      tooltip.style.display = "none";
    }, 2000); // Hide the tooltip after 2 seconds
  }

  // Show the tooltip on page load
  window.addEventListener("load", function () {
    showTooltip();
  });

// Show the tooltip when the icon is clicked
infoIcon.addEventListener("click", function () {
  tooltip.style.display = "block";
});

// Hide the tooltip when the user clicks outside of it
document.addEventListener("click", function (event) {
  if (event.target !== infoIcon && event.target !== tooltip) {
    tooltip.style.display = "none";
  }
});

  const clearTable = () => {
    table.style.display = "none";
    tableBody.innerHTML = "";
    errorMessageDiv.style.display = "none";
    errorMessageDiv.textContent = "";
  };

  // Initial setup: hide the table
  clearTable();

  // Initial state when the page loads
  tableCdsFields.style.visibility = "hidden";
  findObjectSel.style.visibility = "visible";

  radioOptions.forEach((radio) => {
    radio.addEventListener("change", function () {
      if (this.value === "OBJC") {
        findObjectSel.style.visibility = "visible";
        tableCdsFields.style.visibility = "hidden";
        clearTable();
      } else if (this.value === "TABL" || this.value === "CDS") {
        findObjectSel.style.visibility = "hidden";
        tableCdsFields.style.visibility = "visible";
        clearTable();
      }
    });
  });

  const refreshResults = () => {

    clearTable();

    const tcode = tcodeInput.value;
    const program = programInput.value;
    const object = wricef.value;

    const EApiUrl = document.getElementById("api");
    let baseApiUrl;
    
    if (EApiUrl.value === 'SAP') {
      baseApiUrl = 'https://202.142.157.221:51800/zgettab/GetTables?sap-client=901';
    } else {
      baseApiUrl = EApiUrl.value;
    }

    const field1     = Elementfield1.value;
    const field2     = Elementfield2.value;
    const field3     = Elementfield3.value;
    const field4     = Elementfield4.value;

    let apiUrl = "";

    if (
      (!tcode && !program && OBJC.checked) ||
      (!field1 && (TABL.checked || CDS.checked)) ||
      !baseApiUrl //Check if baseApiUrl is empty
    ) {
      // Highlight the input fields
      if (!tcode && OBJC.checked) {
        tcodeInput.classList.add("highlight");
        clearTable();
      }
      if (!field1 && (TABL.checked || CDS.checked)) {
        Elementfield1.classList.add("highlight");
        clearTable();
      }
      if (!baseApiUrl) {
        EApiUrl.classList.add("highlight");
        clearTable();
      }
      return;
    }

    // Set API EndPoint
    if (OBJC.checked) {
      apiUrl = `${baseApiUrl}&TCODE=${tcode}&PGMNA=${program}&OBJECT=${object}`;
    } else if (TABL.checked) {
      apiUrl = `${baseApiUrl}&DDIC=TABL&F1=${field1}&F2=${field2}&F3=${field3}&F4=${field4}`;
    } else if (CDS.checked) {
      apiUrl = `${baseApiUrl}&DDIC=CDS&F1=${field1}&F2=${field2}&F3=${field3}&F4=${field4}`;
    }

    // Disable the button during loading
    submitButton.disabled = true;

    // Show loading sign
    loadingDiv.style.display = "block";
    table.style.display = "none";

    //Remove HighLight
    EApiUrl.classList.remove("highlight");
    Elementfield1.classList.remove("highlight");
    tcodeInput.classList.remove("highlight");

    fetch(apiUrl)
    .then((response) => {
      if (!response.ok) {
        throw new Error(`API request failed with status: ${response.status}`);
      }
      return response.json();
    })
    .then((data) => {
      // Enable the button after loading
      submitButton.disabled = false;
  
      // Hide loading sign and show table
      loadingDiv.style.display = "none";
      table.style.display = "table";
  
      tableBody.innerHTML = "";
  
      if (data && Array.isArray(data)) {
        if (data.length > 0) {
          data.forEach((entry) => {
            const tableRow = document.createElement("tr");
            const ddTextLines = entry.DDTEXT.split('\n'); // Split the text by line breaks
            const ddTextHTML = ddTextLines
              .map((line, index) => `<span class="${index === 2 ? 'result-fields' : ''}">${line}</span><br>`)
              .join('');
            tableRow.innerHTML = `
            <td>${entry.OBJ_NAME}</td>
            <td>${ddTextHTML}</td>
          `;
            tableBody.appendChild(tableRow);
          });
        } else {
          const noResultRow = document.createElement("tr");
          noResultRow.innerHTML = `
            <td colspan="2">Selected object not found for the given TCODE and Program.</td>
          `;
          tableBody.appendChild(noResultRow);
        }
      } else {
        const errorRow = document.createElement("tr");
        errorRow.innerHTML = `
          <td colspan="2">Error: API response is not in the expected format.</td>
        `;
        tableBody.appendChild(errorRow);
      }
    })
    .catch((error) => {
//      console.error("API request error:", error);
      submitButton.disabled = false; // Enable the button on error
      loadingDiv.style.display = "none"; // Hide loading sign on error
      table.style.display = "none"; // Hide the table on error
      errorMessageDiv.style.display = "";
      errorMessageDiv.innerHTML = `API request error: ${error.message}`;

      // SSL error occurred, open the URL in a new tab
//      if (error.message && error.message.includes('net::ERR_CERT_AUTHORITY_INVALID')) {
//        if (response.type === 'opaque' && response.url.startsWith('https://')) {
//        window.location.href = apiUrl;
          window.open(apiUrl, '_blank');
//      }

      const errorRow = document.createElement("tr");
      errorRow.innerHTML = `
        <td colspan="2">API request error: ${error.message}</td>
      `;
      tableBody.appendChild(errorRow);
    });
  };
  
  // Call the refreshResults function when the submit button is clicked
  submitButton.addEventListener("click", function (event) {
    event.preventDefault();
    refreshResults();
  });

  // Call the refreshResults function when the dropdown selection changes
  wricef.addEventListener("change", refreshResults);
});