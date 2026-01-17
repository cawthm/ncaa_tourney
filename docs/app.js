// NCAA Calcutta Auction Tracker - Application Logic

// Configuration loaded from data.json
let CONFIG = null;

// Auction state
const state = {
    prices: {},      // { "East_8_9": 150, ... }
    mine: {},        // { "East_8_9": true, ... }
    budget: 2000
};

// Storage key
const STORAGE_KEY = 'ncaa_auction_tracker';

// Initialize the app
async function init() {
    try {
        const response = await fetch('data.json');
        CONFIG = await response.json();
    } catch (e) {
        console.error('Failed to load config:', e);
        // Use inline fallback
        CONFIG = getDefaultConfig();
    }

    loadState();
    buildAuctionGrid();
    buildTargetsGrid();
    updateAll();
    setupEventListeners();
}

function getDefaultConfig() {
    return {
        matchups: [
            {id: "8_9", label: "8/9"},
            {id: "7_10", label: "7/10"},
            {id: "6_11", label: "6/11"},
            {id: "5_12", label: "5/12"},
            {id: "4_13", label: "4/13"},
            {id: "3_14", label: "3/14"},
            {id: "2_15", label: "2/15"},
            {id: "1_16", label: "1/16"}
        ],
        regions: ["East", "South", "West", "Midwest"],
        expectedValues: {
            "8_9": 1.27, "7_10": 1.48, "6_11": 1.78, "5_12": 1.72,
            "4_13": 2.40, "3_14": 2.93, "2_15": 4.75, "1_16": 8.75
        },
        historicalPriceRatios: {
            "8_9": 1.0, "7_10": 1.15, "6_11": 1.35, "5_12": 1.30,
            "4_13": 1.75, "3_14": 2.10, "2_15": 3.50, "1_16": 6.50
        }
    };
}

// Build the auction grid
function buildAuctionGrid() {
    const grid = document.querySelector('.auction-grid');

    CONFIG.matchups.forEach(matchup => {
        // Matchup label
        const label = document.createElement('div');
        label.className = 'matchup-label';
        label.textContent = matchup.label;
        grid.appendChild(label);

        // Region cells
        CONFIG.regions.forEach(region => {
            const cell = document.createElement('div');
            cell.className = 'auction-cell';
            cell.dataset.region = region;
            cell.dataset.matchup = matchup.id;

            const key = `${region}_${matchup.id}`;

            cell.innerHTML = `
                <input type="number" class="price-input" placeholder="$0"
                       data-key="${key}" value="${state.prices[key] || ''}">
                <div class="ev-hint">EV: ${CONFIG.expectedValues[matchup.id]}%</div>
                <label>
                    <input type="checkbox" class="mine-checkbox" data-key="${key}"
                           ${state.mine[key] ? 'checked' : ''}>
                    Mine
                </label>
            `;

            grid.appendChild(cell);
        });
    });
}

// Build the targets grid
function buildTargetsGrid() {
    const grid = document.querySelector('.targets-grid');

    CONFIG.matchups.forEach(matchup => {
        const ev = CONFIG.expectedValues[matchup.id];

        const cells = `
            <div class="target-cell matchup">${matchup.label}</div>
            <div class="target-cell">${ev}%</div>
            <div class="target-cell target-value" data-matchup="${matchup.id}" data-type="target">$0</div>
            <div class="target-cell max-value" data-matchup="${matchup.id}" data-type="max">$0</div>
        `;

        grid.insertAdjacentHTML('beforeend', cells);
    });
}

// Set up event listeners
function setupEventListeners() {
    // Price inputs
    document.querySelectorAll('.price-input').forEach(input => {
        input.addEventListener('change', handlePriceChange);
        input.addEventListener('input', handlePriceChange);
    });

    // Mine checkboxes
    document.querySelectorAll('.mine-checkbox').forEach(checkbox => {
        checkbox.addEventListener('change', handleMineChange);
    });

    // Budget input
    document.getElementById('budget-input').addEventListener('change', handleBudgetChange);
    document.getElementById('budget-input').addEventListener('input', handleBudgetChange);

    // Clear button
    document.getElementById('clear-btn').addEventListener('click', handleClear);

    // Export button
    document.getElementById('export-btn').addEventListener('click', handleExport);

    // Import button
    document.getElementById('import-btn').addEventListener('click', () => {
        document.getElementById('import-file').click();
    });
    document.getElementById('import-file').addEventListener('change', handleImport);
}

// Handle price change
function handlePriceChange(e) {
    const key = e.target.dataset.key;
    const value = parseFloat(e.target.value) || 0;

    if (value > 0) {
        state.prices[key] = value;
    } else {
        delete state.prices[key];
    }

    updateCellStyle(e.target.closest('.auction-cell'), key);
    updateAll();
    saveState();
}

// Handle mine checkbox change
function handleMineChange(e) {
    const key = e.target.dataset.key;
    state.mine[key] = e.target.checked;

    updateCellStyle(e.target.closest('.auction-cell'), key);
    updateAll();
    saveState();
}

// Handle budget change
function handleBudgetChange(e) {
    state.budget = parseFloat(e.target.value) || 0;
    updateAll();
    saveState();
}

// Update cell styling
function updateCellStyle(cell, key) {
    const price = state.prices[key];
    const isMine = state.mine[key];
    const matchupId = key.split('_').slice(1).join('_');
    const estPot = estimateFinalPot();
    const ev = (CONFIG.expectedValues[matchupId] / 100) * estPot;

    cell.classList.remove('sold', 'yours', 'over-ev');

    if (price > 0) {
        cell.classList.add('sold');
        if (isMine) {
            cell.classList.add('yours');
        }
        if (price > ev * 1.2) {
            cell.classList.add('over-ev');
        }
    }
}

// Calculate current pot total
function calculateCurrentPot() {
    return Object.values(state.prices).reduce((sum, price) => sum + price, 0);
}

// Count items sold
function countItemsSold() {
    return Object.keys(state.prices).filter(k => state.prices[k] > 0).length;
}

// Estimate final pot based on items sold
function estimateFinalPot() {
    const itemsSold = countItemsSold();
    const currentPot = calculateCurrentPot();

    if (itemsSold === 0) return 20000; // Default estimate
    if (itemsSold === 32) return currentPot;

    // Calculate expected remaining based on what's been sold
    let soldRatioSum = 0;
    let remainingRatioSum = 0;

    CONFIG.matchups.forEach(matchup => {
        CONFIG.regions.forEach(region => {
            const key = `${region}_${matchup.id}`;
            const ratio = CONFIG.historicalPriceRatios[matchup.id];

            if (state.prices[key] > 0) {
                soldRatioSum += ratio;
            } else {
                remainingRatioSum += ratio;
            }
        });
    });

    if (soldRatioSum === 0) return 20000;

    // Average price per ratio unit
    const pricePerRatioUnit = currentPot / soldRatioSum;

    // Estimate remaining
    const estimatedRemaining = pricePerRatioUnit * remainingRatioSum;

    return Math.round(currentPot + estimatedRemaining);
}

// Calculate spent on "mine" items
function calculateSpent() {
    return Object.keys(state.mine)
        .filter(k => state.mine[k])
        .reduce((sum, k) => sum + (state.prices[k] || 0), 0);
}

// Calculate expected payout from "mine" items
function calculateExpectedPayout() {
    const estPot = estimateFinalPot();

    return Object.keys(state.mine)
        .filter(k => state.mine[k])
        .reduce((sum, k) => {
            const matchupId = k.split('_').slice(1).join('_');
            const ev = CONFIG.expectedValues[matchupId] / 100;
            return sum + (ev * estPot);
        }, 0);
}

// Get list of purchases
function getPurchases() {
    return Object.keys(state.mine)
        .filter(k => state.mine[k] && state.prices[k] > 0)
        .map(k => {
            const [region, ...matchupParts] = k.split('_');
            const matchupId = matchupParts.join('_');
            const matchup = CONFIG.matchups.find(m => m.id === matchupId);
            const estPot = estimateFinalPot();
            const ev = (CONFIG.expectedValues[matchupId] / 100) * estPot;

            return {
                key: k,
                region,
                matchup: matchup ? matchup.label : matchupId,
                price: state.prices[k],
                ev
            };
        });
}

// Update all displays
function updateAll() {
    const itemsSold = countItemsSold();
    const currentPot = calculateCurrentPot();
    const estPot = estimateFinalPot();
    const spent = calculateSpent();
    const remaining = state.budget - spent;
    const expectedPayout = calculateExpectedPayout();

    // Summary
    document.getElementById('items-sold').textContent = `${itemsSold} / 32`;
    document.getElementById('current-pot').textContent = formatCurrency(currentPot);
    document.getElementById('est-pot').textContent = formatCurrency(estPot);
    document.getElementById('spent').textContent = formatCurrency(spent);
    document.getElementById('remaining').textContent = formatCurrency(remaining);
    document.getElementById('remaining').style.color = remaining < 0 ? 'var(--danger)' : '';

    // Targets
    document.getElementById('target-pot').textContent = formatCurrency(estPot);

    CONFIG.matchups.forEach(matchup => {
        const ev = CONFIG.expectedValues[matchup.id];
        const target = Math.round((ev / 100) * estPot);
        const max = Math.round(target * 1.2);

        const targetCell = document.querySelector(`.target-cell[data-matchup="${matchup.id}"][data-type="target"]`);
        const maxCell = document.querySelector(`.target-cell[data-matchup="${matchup.id}"][data-type="max"]`);

        if (targetCell) targetCell.textContent = formatCurrency(target);
        if (maxCell) maxCell.textContent = formatCurrency(max);
    });

    // Purchases
    const purchases = getPurchases();
    const purchasesList = document.getElementById('purchases-list');

    if (purchases.length === 0) {
        purchasesList.innerHTML = '<p class="empty-msg">Mark items as "yours" by clicking the checkbox next to the price.</p>';
    } else {
        purchasesList.innerHTML = purchases.map(p => `
            <div class="purchase-item">
                <span class="name">${p.region} ${p.matchup}</span>
                <span class="price">Paid: ${formatCurrency(p.price)}</span>
                <span class="ev">EV: ${formatCurrency(p.ev)}</span>
            </div>
        `).join('');
    }

    document.getElementById('your-total').textContent = formatCurrency(spent);
    document.getElementById('your-ev').textContent = formatCurrency(expectedPayout);

    const roi = spent > 0 ? ((expectedPayout - spent) / spent * 100) : 0;
    document.getElementById('your-roi').textContent = `${roi >= 0 ? '+' : ''}${roi.toFixed(1)}%`;
    document.getElementById('your-roi').style.color = roi >= 0 ? 'var(--success)' : 'var(--danger)';

    // Update cell styles
    document.querySelectorAll('.auction-cell').forEach(cell => {
        const key = `${cell.dataset.region}_${cell.dataset.matchup}`;
        updateCellStyle(cell, key);
    });
}

// Format currency
function formatCurrency(value) {
    return '$' + Math.round(value).toLocaleString();
}

// Save state to localStorage
function saveState() {
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    } catch (e) {
        console.warn('Failed to save state:', e);
    }
}

// Load state from localStorage
function loadState() {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const parsed = JSON.parse(saved);
            Object.assign(state, parsed);

            // Update budget input
            document.getElementById('budget-input').value = state.budget;
        }
    } catch (e) {
        console.warn('Failed to load state:', e);
    }
}

// Handle clear all
function handleClear() {
    if (confirm('Clear all auction data? This cannot be undone.')) {
        state.prices = {};
        state.mine = {};

        document.querySelectorAll('.price-input').forEach(input => {
            input.value = '';
        });

        document.querySelectorAll('.mine-checkbox').forEach(checkbox => {
            checkbox.checked = false;
        });

        updateAll();
        saveState();
    }
}

// Handle export
function handleExport() {
    const data = {
        timestamp: new Date().toISOString(),
        state: state
    };

    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = `auction_${new Date().toISOString().split('T')[0]}.json`;
    a.click();

    URL.revokeObjectURL(url);
}

// Handle import
function handleImport(e) {
    const file = e.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = function(event) {
        try {
            const data = JSON.parse(event.target.result);

            if (data.state) {
                Object.assign(state, data.state);

                // Update UI
                document.getElementById('budget-input').value = state.budget;

                document.querySelectorAll('.price-input').forEach(input => {
                    const key = input.dataset.key;
                    input.value = state.prices[key] || '';
                });

                document.querySelectorAll('.mine-checkbox').forEach(checkbox => {
                    const key = checkbox.dataset.key;
                    checkbox.checked = state.mine[key] || false;
                });

                updateAll();
                saveState();

                alert('Data imported successfully!');
            }
        } catch (err) {
            alert('Failed to import data: ' + err.message);
        }
    };

    reader.readAsText(file);
    e.target.value = ''; // Reset file input
}

// Initialize on DOM ready
document.addEventListener('DOMContentLoaded', init);
